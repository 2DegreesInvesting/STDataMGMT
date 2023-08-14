

#' Prepare raw input data files from Eikon
#'
#' @param ids ids db
#' @param assets assets db
#' @param companies companies db
#'
#' @return expected eikon input data

prepare_eikon_data_input <- function(ids, assets, companies) {
  table_keys <- ids %>%
    filter(!is.na(company_id),!is.na(isin)) %>%
    distinct(company_id, isin)

  eikon_data <- dplyr::inner_join(
    dplyr::inner_join(table_keys, assets, by = "isin"),
    dplyr::inner_join(table_keys, companies, by = "company_id"),
    by = c("isin", "company_id")
  ) %>%
    dplyr::rename(
      parent_company_id=company_id,
      debt_equity_ratio = .data$credit_structural_leverage,
      pd = .data$`credit_structural_pd_%`,
      net_profit_margin = .data$`net_profit_margin_%`,
      volatility = .data$`credit_structural_asset_volatility_%`
    ) %>%
    dplyr::mutate(
      net_profit_margin = .data$net_profit_margin / 100,
      volatility = .data$volatility / 100,
      pd = .data$pd / 100
    ) %>%
    select(
      c(
        "isin",
        "parent_company_id",
        "is_ultimate_parent",
        "company_name",
        "country_of_domicile",
        "bics_sector",
        "bics_subgroup",
        "company_market_cap",
        "volatility",
        "credit_structural_asset_drift_%",
        "pd",
        "debt_equity_ratio",
        "net_profit_margin",
        "total_debt"
      )
    )
  # filter rows where all eikon numerical columns are NA
  eikon_data <- eikon_data %>%
    filter(
      !(is.na(debt_equity_ratio)
      & is.na(pd)
      & is.na(net_profit_margin)
      & is.na(volatility))) %>%
    report_diff_rows(initial_n_rows = nrow(eikon_data),
                     cause = "Because no Eikon features provided for those ISINs")

  return(data)
}

#' Prewrangle the ownership tree data from AR
#'
#' @param data A data frame holding the raw ownership tree data set
#' @return NULL
prewrangle_ownership_tree <- function(data) {
  # filter only one direction within ownership_tree
  ownership_tree <- data %>%
    dplyr::filter(.data$ownership_level >= 0) %>%
    report_diff_rows(initial_n_rows = nrow(data),
                     cause = "because of the wrong direction in the ownership tree")

  # only take the majority parent for each company at each ownership level
  # (otherwise it would get the profit margins from both parents, which one is the better one??)
  # Note: if linking_stake == NA in raw data, this means 100% is owned by one company
  ownership_tree <- ownership_tree %>%
    # need to change NAs to 100%, otherwise slice_max will kick them out
    dplyr::mutate(linking_stake = dplyr::if_else(is.na(.data$linking_stake), 100, .data$linking_stake)) %>%
    dplyr::group_by(.data$company_id, .data$ownership_level) %>%
    # slice the majority parent
    dplyr::slice_max(.data$linking_stake) %>%
    dplyr::ungroup() %>%
    # still have to take distinct in case the linking stake is equal (e.g. 50%/50%)
    dplyr::distinct(.data$company_id, .data$ownership_level, .keep_all = TRUE) %>%
    report_diff_rows(initial_n_rows = nrow(ownership_tree),
                     cause = "by choosing only the majority parent")
}

#' Identify which companies in the masterdata_ownership are missing in Eikon
#'
#' @param data A data frame holding the masterdata file in which to find the missings
#' @param eikon_data A data frame holding prepared Eikon data set, containing company_id,
#'   company_name and corporate_bond_ticker
#'
#' @return NULL
find_missing_companies_ownership <- function(data, eikon_data) {
  # take distinct company ids
  # identify missing companies by keeping only companies in master data, that cannot be found in eikon
  # (bloomberg_id would be better but it somehow has NAs in the master data while company name does not)

  data_missing_companies <- data %>%
    dplyr::distinct(.data$company_id) %>%
    dplyr::anti_join(eikon_data, by = "company_id") %>%
    report_diff_rows(initial_n_rows = nrow(data),
                     cause = "because the other companies were already present in the Eikon Data")
  return(data_missing_companies)
}

#' Identify which companies in the abcd_stress_test_input are missing in Eikon
#'
#' @param data A data frame holding prepared Eikon data set
#' @param missing_companies A data frame with missing companies from
#'   the masterdata_ownership data set
#'
#' @return NULL
add_missing_companies_to_eikon <- function(data,
                                           missing_companies) {
  # ensure additional company IDs are unique (e.g. some corporate bond tickers
  # will also belong to companies in the ownership data)
  missing_companies <- missing_companies %>%
    dplyr::distinct(.data$company_id, .keep_all = TRUE) %>%
    report_diff_rows(initial_n_rows = nrow(missing_companies),
                     cause = "because of non unique company IDs")

  # add to Eikon data
  eikon_data <- data %>%
    dplyr::bind_rows(missing_companies) %>%
    report_diff_rows(initial_n_rows = nrow(data),
                     cause = "by adding the missing companies from AR")
}

#' Adds information on where the company information in the processed Eikon data
#' set comes from.
#'
#' @param data A data frame holding processed Eikon data
#' #'
#' @return A data frame
add_company_source_info <- function(data) {
  data <- data %>%
    dplyr::mutate(
      source_company = dplyr::case_when(
        .data$company_id == .data$parent_company_id ~ "Eikon company",
        .data$company_id != .data$parent_company_id &
          !is.na(.data$parent_company_id) ~ "Subsidiary of eikon company",
        TRUE ~ "AR company"
      )
    )
}


#' Create averages of eikon variables for companies with missing values
#'
#' @description This function uses the median for averaging numeric variables
#' because the results may otherwise be very sensitive to outliers. Averages are
#' created only based on values we previously obtained directly from eikon.
#' @param data A data frame holding processed and possibly filtered Eikon data
#' @param minimum_sample_size A numeric vector of length one that determines the
#'   minimum required absolute sample size of the given subgroup. Below this
#'   value we do not calculate an average for this sub group.
#' @param minimum_ratio_sample A numeric vector of length one that determines
#'   the minimum required ratio of sub group to overall sample. Below this
#'   value we do not calculate an average for this sub group.
#' @param allowed_range_npm A numeric vector indicating the allowed minimum and
#'   maximum values for the net profit margin
#'
#' @return A data frame
create_averages_eikon <- function(data,
                                  minimum_sample_size,
                                  minimum_ratio_sample,
                                  allowed_range_npm) {
  data <- data %>%
    dplyr::mutate(size_subgroup = dplyr::n()) %>%
    dplyr::filter(.data$source_company == "Eikon company") %>%
    dplyr::summarise(
      size_subgroup = mean(.data$size_subgroup, na.rm = TRUE),
      size_sample = dplyr::n(),
      ratio_sample_subgroup = .data$size_sample / .data$size_subgroup,
      sample_sufficient = dplyr::if_else(.data$size_sample > .env$minimum_sample_size, TRUE, FALSE),
      ratio_sufficient = dplyr::if_else(
        .data$ratio_sample_subgroup > .env$minimum_ratio_sample,
        TRUE,
        FALSE
      ),
      # avg_asset_drift = stats::median(.data$asset_drift, na.rm = TRUE),
      avg_pd = stats::median(.data$pd, na.rm = T),
      avg_volatility = stats::median(.data$volatility, na.rm = T),
      avg_net_profit_margin = stats::median(.data$net_profit_margin, na.rm = T),
      avg_debt_equity_ratio = stats::median(.data$debt_equity_ratio, na.rm = T),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(dplyr::across(dplyr::contains("avg_"), ~ (!is.na(.))),
                  dplyr::across(dplyr::contains("avg_"), ~ (!is.infinite(.)))) %>%
    dplyr::filter(.data$sample_sufficient == TRUE |
                    .data$ratio_sufficient == TRUE) %>%
    dplyr::filter(dplyr::between(
      .data$avg_net_profit_margin,
      min(.env$allowed_range_npm, na.rm = TRUE),
      max(.env$allowed_range_npm, na.rm = TRUE)
    ))
}

#' Plot the imputed set of average values by a certain level of granularity
#'
#' @param data A data frame which contains the created averages for a
#'   combination of sectors and regions
#' @param x Variable to display on the x axis
#' @param y Variable to display on the y axis
#' @param subregion Logical parameter indicates whether to display sub regions
#'   by color or not. Default is FALSE.
#'
#' @return A ggplot2 object
plot_sector_averages <- function(data, x, y, subregion = FALSE) {
  if (subregion) {
    plot <- data %>%
      ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))) +
      ggplot2::geom_point(ggplot2::aes(col = subregion),
                          size = 4,
                          alpha = 0.6) +
      ggplot2::theme_light() +
      ggplot2::coord_flip() +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
  } else {
    plot <- data %>%
      ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))) +
      ggplot2::geom_point(size = 4, alpha = 0.6) +
      ggplot2::theme_light() +
      ggplot2::coord_flip() +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
  }
}

#' For each company select the final financial value of the best granularity
#'
#' @param data A data frame which contains the financial data including the
#'   calculated averages for a combination of sectors and regions
#'
#' @return A data frame
select_final_financial_value <- function(data) {
  # pivot long company info + indicators from eikon
  eikon_values_long <- data %>%
    dplyr::select(-names(data)[stringr::str_detect(names(data), "avg_")]) %>%
    tidyr::pivot_longer(
      cols = stringr::str_remove_all(names(data)[stringr::str_detect(names(data), "avg_")], "avg_"),
      values_to = "eikon",
      values_transform = list(eikon = as.character)
    )
  # pivot long averages
  average_values_long <- data %>%
    dplyr::select(.data$company_id, names(data)[stringr::str_detect(names(data), "avg_")]) %>%
    tidyr::pivot_longer(
      cols = !"company_id",
      values_to = "avg",
      values_transform = list(avg = as.character)
    ) %>%
    dplyr::mutate(name = stringr::str_remove_all(.data$name, "avg_"))

  # join both long formats
  eikon_data_long <- eikon_values_long %>%
    dplyr::left_join(
      average_values_long,
      by = c("name", "company_id"),
      suffix = c("_eikon", "_avg")
    )

  # choose final indicators + determine data types
  eikon_data_long <- eikon_data_long %>%
    dplyr::mutate(
      final = dplyr::if_else(!is.na(.data$eikon), .data$eikon, .data$avg),
      final_indicator_type = paste(
        "Financial indicator from",
        dplyr::if_else(
          !is.na(.data$eikon),
          "Eikon",
          paste(.data$average_type, "average")
        )
      ),
      overall_data_type = paste(.data$source_company, .data$final_indicator_type, sep = " | ")
    )

  # order data types based on assumed accuracy
  eikon_data_long <- eikon_data_long %>%
    dplyr::mutate(overall_data_type = factor(
      .data$overall_data_type,
      levels = c(
        "Eikon company | Financial indicator from Eikon",
        "Subsidiary of eikon company | Financial indicator from Eikon",
        "Eikon company | Financial indicator from bics_subgroup_region average",
        "Eikon company | Financial indicator from ald_sector_region average",
        "Eikon company | Financial indicator from bics_subgroup average",
        "Eikon company | Financial indicator from ald_sector average",
        "Eikon company | Financial indicator from global average",
        "Subsidiary of eikon company | Financial indicator from bics_subgroup_region average",
        "Subsidiary of eikon company | Financial indicator from ald_sector_region average",
        "Subsidiary of eikon company | Financial indicator from bics_subgroup average",
        "Subsidiary of eikon company | Financial indicator from ald_sector average",
        "Subsidiary of eikon company | Financial indicator from global average",
        "AR company | Financial indicator from bics_subgroup_region average",
        "AR company | Financial indicator from ald_sector_region average",
        "AR company | Financial indicator from bics_subgroup average",
        "AR company | Financial indicator from ald_sector average",
        "AR company | Financial indicator from global average"
      )
    ))

  # pivot back to wide format
  data <- eikon_data_long %>%
    tidyr::pivot_wider(
      names_from = .data$name,
      names_sep = "_",
      values_from = c(
        .data$eikon,
        .data$avg,
        .data$final,
        .data$final_indicator_type,
        .data$overall_data_type
      )
    ) %>%
    assertr::verify(nrow(.) == nrow(data))

  # change variable classes back to doubles
  data <- data %>%
    dplyr::mutate(dplyr::across(c(
      dplyr::contains(c("final_", "eikon_", "avg_")),-dplyr::contains(c("_type_", "structural"))
    ),
    ~ as.double(.)))
}

#' For each company select the final financial value of the best granularity
#'
#' @param ids ids db
#' @param companies companies db
#' @param assets assets db
#' @param ownership_tree A data frame containing the ownership_tree as provided
#'   by Asset Resolution
#' @param country_region_bridge A data frame which contains a mapping of
#'   countries to multiple regional levels of interest
#' @param n_min_sample A numeric vector of length one, indicating the minimum
#'   required size of the reference subgroups when calculating averages
#' @param min_ratio_sample_subgroup A numeric vector of length one, indicating
#'   the minimum required share of the reference subgroups compared to the full
#'   sample size when calculating averages
#' @param range_profit_margin A numeric vector indicating the allowed minimum
#'   and maximum values for the net profit margin when calculating the averages
#'
#' @return A data frame
prepare_eikon_data <- function(companies,
                               assets,
                               ownership_tree,
                               country_region_bridge,
                               n_min_sample,
                               min_ratio_sample_subgroup,
                               range_profit_margin) {
  # 1) Eikon preparation / pre wrangling----
  eikon_data <- prepare_eikon_data_input(ids, assets, companies)

  # 2) Expand eikon data across ownership tree and bond_tickers-----
  # join in ownership tree----
  eikon_data <- eikon_data %>%
    # ADO 1948 - apparently no diff between left_join and inner_join, so changed to the more commonly correct one
    dplyr::left_join(ownership_tree,
                     by = c("parent_company_id" = "target_company_id")) %>%
    dplyr::mutate(
      company_id = dplyr::if_else(
        is.na(.data$company_id),
        .data$parent_company_id,
        .data$company_id
      ),
      ownership_level = dplyr::if_else(is.na(.data$ownership_level), 0, .data$ownership_level)
    )

  # rm(ownership_tree, ownership_tree)

  # ensure that company_ids are distinct----
  # ... but consider the most granular eikon
  # data points (e.g. take profit margin of child if available)

  # ensure that each company has a ownership level (slice_min kicks out NAs)
  eikon_data <- eikon_data %>%
    dplyr::filter(!is.na(.data$ownership_level)) %>%
    assertr::verify(nrow(.) == nrow(eikon_data))

  # take "closest" profit margins (i.e. own profit margin if available or from
  # the closest parent)
  # For reference: ownership_level == 0 when the company_id is the parent_company_id
  eikon_data <- eikon_data %>%
    dplyr::group_by(.data$company_id) %>%
    dplyr::slice_min(.data$ownership_level) %>%
    dplyr::ungroup() %>%
    report_diff_rows(initial_n_rows = nrow(eikon_data),
                     cause = "in order to only consider the closest information")

  # 3) Add companies which have ALD but no eikon data (these will obtain averages)----

  # asset_impact_missing_companies----
  abcd_data_missing_companies <- abcd_data %>%
    find_missing_companies_ownership(eikon_data = eikon_data)

  # add additional companies to eikon data----
  eikon_data <- eikon_data %>%
    add_missing_companies_to_eikon(missing_companies = abcd_data_missing_companies)

  # for each company ID in the processed eikon data set, define the source of that
  # company (also used as an indicator later on)
  eikon_data <- eikon_data %>%
    add_company_source_info()

  # 4) add further company information------

  # add security mapped sector (this determines the final sector in pacta and can
  # differ from financial sector)
  security_financial_data_sector_classifications <-
    companies %>%
    dplyr::filter(.data$ald_sector != "Other") %>%
    dplyr::distinct(.data$company_id, .keep_all = TRUE) %>%
    dplyr::select(.data$company_id,
                  .data$ald_sector)

  eikon_data <- eikon_data %>%
    dplyr::left_join(security_financial_data_sector_classifications,
                     by = "company_id") %>%
    dplyr::mutate(
      ald_sector = dplyr::if_else(
        is.na(.data$ald_sector),
        "Other",
        .data$ald_sector
      )
    ) %>%
    assertr::verify(nrow(.) == nrow(eikon_data))

  # join in regional bridge for country_of_domicile
  eikon_data <- eikon_data %>%
    dplyr::left_join(country_region_bridge,
                     by = c("country_of_domicile" = "iso_a2")) %>%
    assertr::verify(nrow(.) == nrow(eikon_data))

  # arrange columns
  eikon_data <- eikon_data %>%
    dplyr::select(
      .data$company_name,
      .data$country_of_domicile,
      # .data$financial_sector,
      .data$ald_sector,
      .data$bics_sector,
      .data$bics_subgroup,
      .data$company_id,
      # .data$bloomberg_id,
      .data$corporate_bond_ticker,
      # .data$is_ultimate_listed_parent,
      .data$is_ultimate_parent,
      # .data$market_cap,
      .data$parent_company_id,
      .data$ownership_level,
      dplyr::everything(),
      -.data$linking_stake
    )

  # ensure unique company names because will be used to join later
  # as they are not unique, we just have to gamble that we take the right one
  eikon_data <- eikon_data %>%
    # arrange by CB ticker to improve CB ticker coverage (often only one of the
    # duplicates has a CB ticker)
    dplyr::arrange(.data$corporate_bond_ticker) %>%
    dplyr::distinct(.data$company_name, .keep_all = TRUE) %>%
    report_diff_rows(initial_n_rows = nrow(eikon_data),
                     cause = "to ensure unique company names")

  # only filter companies with ALD --> kick out out irrelevant eikon companies or
  # irrelevant added subsidiaries
  # eikon_data <- eikon_data %>%
  #   rm_companies_without_abcd(
  #     md_ownership = masterdata_ownership,
  #     md_debt = masterdata_debt,
  #     md_credit = masterdata_credit
  #   )

  # 5) create averages by different granularities of sub groups----
  # (but only based on values we obtained directly from eikon)

  # create averages by bics subsector and region
  bics_subgroup_region_averages <- eikon_data %>%
    dplyr::filter(!is.na(.data$bics_subgroup),!is.na(.data$subregion)) %>%
    dplyr::group_by(.data$bics_subgroup, .data$subregion) %>%
    create_averages_eikon(
      minimum_sample_size = n_min_sample,
      minimum_ratio_sample = min_ratio_sample_subgroup,
      allowed_range_npm = range_profit_margin
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(average_type = "bics_subgroup_region")

  # create averages by ald_sector and region
  ald_sector_region_averages <- eikon_data %>%
    dplyr::filter(!is.na(.data$ald_sector),!is.na(.data$subregion)) %>%
    dplyr::group_by(.data$ald_sector, .data$subregion) %>%
    create_averages_eikon(
      minimum_sample_size = n_min_sample,
      minimum_ratio_sample = min_ratio_sample_subgroup,
      allowed_range_npm = range_profit_margin
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(average_type = "ald_sector_region")

  # create global bics_subgroup average
  bics_subgroup_averages <- eikon_data %>%
    dplyr::filter(!is.na(.data$bics_subgroup)) %>%
    dplyr::group_by(.data$bics_subgroup) %>%
    create_averages_eikon(
      minimum_sample_size = n_min_sample,
      minimum_ratio_sample = min_ratio_sample_subgroup,
      allowed_range_npm = range_profit_margin
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(average_type = "bics_subgroup")

  # create global ald_sector average
  ald_sector_averages <- eikon_data %>%
    dplyr::filter(!is.na(.data$ald_sector)) %>%
    dplyr::group_by(.data$ald_sector) %>%
    create_averages_eikon(
      minimum_sample_size = n_min_sample,
      minimum_ratio_sample = min_ratio_sample_subgroup,
      allowed_range_npm = range_profit_margin
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(average_type = "ald_sector")

  # create global average
  global_averages <- eikon_data %>%
    create_averages_eikon(
      minimum_sample_size = n_min_sample,
      minimum_ratio_sample = min_ratio_sample_subgroup,
      allowed_range_npm = range_profit_margin
    ) %>%
    dplyr::mutate(average_type = "global")


  # create plots for sub group averages----
  plot_bics_subgroup_region_averages <-
    bics_subgroup_region_averages %>%
    # dplyr::filter(dplyr::between(.data$avg_profit_margin_preferred, -2,2)) %>%
    plot_sector_averages(x = "bics_subgroup",
                         y = "avg_net_profit_margin",
                         subregion = TRUE)

  plot_ald_sector_region_averages <-
    ald_sector_region_averages %>%
    # dplyr::filter(dplyr::between(.data$avg_net_profit_margin, -2,2)) %>%
    plot_sector_averages(x = "ald_sector",
                         y = "avg_net_profit_margin",
                         subregion = TRUE)

  plot_bics_subgroup_averages <- bics_subgroup_averages %>%
    dplyr::filter(dplyr::between(.data$avg_net_profit_margin,-2, 2)) %>%
    plot_sector_averages(x = "bics_subgroup",
                         y = "avg_net_profit_margin")

  plot_ald_sector_averages <-
    ald_sector_averages %>%
    # dplyr::filter(dplyr::between(.data$avg_profit_margin_preferred, -2,2)) %>%
    plot_sector_averages(x = "ald_sector",
                         y = "avg_net_profit_margin")

  # add most the most granular average calculated to the eikon data----
  # subset companies for which we have good bics_subgroup + regional averages available
  eikon_data_bics_subgroup_region_averages <- eikon_data %>%
    dplyr::inner_join(bics_subgroup_region_averages,
                      by = c("bics_subgroup", "subregion"))

  # subset companies for which we have good ald_sector + regional averages available
  eikon_data_ald_sector_region_averages <-
    eikon_data %>%
    dplyr::anti_join(eikon_data_bics_subgroup_region_averages, by = "company_id") %>%
    dplyr::inner_join(
      ald_sector_region_averages,
      by = c("ald_sector", "subregion")
    )

  # subset companies for which we have good bics_subgroup averages available + which havent subset beforehand
  eikon_data_bics_subgroup_averages <- eikon_data %>%
    dplyr::anti_join(eikon_data_bics_subgroup_region_averages, by = "company_id") %>%
    dplyr::anti_join(eikon_data_ald_sector_region_averages, by = "company_id") %>%
    dplyr::inner_join(bics_subgroup_averages, by = "bics_subgroup")

  # subset companies for which we have good ald_sector averages available + which havent subset beforehand
  eikon_data_ald_sector_averages <- eikon_data %>%
    dplyr::anti_join(eikon_data_bics_subgroup_region_averages, by = "company_id") %>%
    dplyr::anti_join(eikon_data_ald_sector_region_averages, by = "company_id") %>%
    dplyr::anti_join(eikon_data_bics_subgroup_averages, by = "company_id") %>%
    dplyr::inner_join(ald_sector_averages, by = "ald_sector")

  # add global averages for companies which havent obtained averages beforehand
  eikon_data_global_averages <- eikon_data %>%
    dplyr::anti_join(eikon_data_bics_subgroup_region_averages, by = "company_id") %>%
    dplyr::anti_join(eikon_data_ald_sector_region_averages, by = "company_id") %>%
    dplyr::anti_join(eikon_data_bics_subgroup_averages, by = "company_id") %>%
    dplyr::anti_join(eikon_data_ald_sector_averages, by = "company_id") %>%
    dplyr::bind_cols(global_averages)

  # bind together
  eikon_data <- rbind.data.frame(
    eikon_data_bics_subgroup_region_averages,
    eikon_data_ald_sector_region_averages,
    eikon_data_bics_subgroup_averages,
    eikon_data_ald_sector_averages,
    eikon_data_global_averages
  ) %>%
    assertr::verify(nrow(.) == nrow(eikon_data))

  # rm(
  #   n_min_sample, min_ratio_sample_subgroup, range_profit_margin,
  #   eikon_data_bics_subgroup_region_averages, bics_subgroup_region_averages,
  #   bics_subgroup_averages, ald_sector_region_averages,
  #   ald_sector_averages, eikon_data_bics_subgroup_averages,
  #   eikon_data_global_averages, eikon_data_ald_sector_region_averages,
  #   eikon_data_ald_sector_averages, global_averages
  # )

  # # create new avg rating based on avg PDs. Rating boundaries based on input data
  # # TODO: ADO 3542 - this currently throws warnings. we do not use the variable
  # # though so we can fix it later
  # for (e in unique(eikon_data$structural)) {
  #   eikon_data <- eikon_data %>%
  #     dplyr::mutate(
  #       avg_structural =
  #         dplyr::case_when(
  #           dplyr::between(
  #             avg_pd,
  #             eikon_data %>% dplyr::filter(structural == e) %>% dplyr::summarise(min(pd, na.rm = TRUE)),
  #             eikon_data %>% dplyr::filter(structural == e) %>% dplyr::summarise(max(pd, na.rm = TRUE))
  #           ) & avg_structural == "NA" ~ e,
  #           TRUE ~ avg_structural
  #         )
  #     )
  #
  #   rm(e)
  # }

  # ensure that no NAs and Inf are in the avg data
  eikon_data %>%
    dplyr::filter(dplyr::if_any(dplyr::contains("avg_"), ~ (is.na(.))) |
                    dplyr::if_any(dplyr::contains("avg_"), ~ (is.infinite(.)))) %>%
    assertr::verify(nrow(.) == 0)

  # 6) pick the best financial data point available----
  eikon_data <- eikon_data %>%
    select_final_financial_value() %>%
    assertr::verify(length(unique(.$company_id)) == length(unique(asset_impact_data$company_id)))

  # 7) apply financial data values cleaning assumptions
  eikon_data <- eikon_data %>%
    prepare_prewrangled_financial_data_stress_test()

  return(eikon_data)
}

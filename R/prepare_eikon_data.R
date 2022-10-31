#' Prewrangle the raw master data credit input file from AR
#'
#' @param masterdata_credit A data frame holding the raw master data credit data
#' @param consolidated_financial_data A data frame holding
#'   consolidated_financial_data
#'
#' @return NULL
prewrangle_masterdata_credit <- function(masterdata_credit,
                                         consolidated_financial_data) {
  masterdata_credit <- masterdata_credit %>%
    tidyr::pivot_longer(
      cols = tidyr::starts_with("_"),
      names_to = "year",
      names_prefix = "_",
      values_to = "ald_production"
    ) %>%
    dplyr::rename(
      id = .data$company_id,
      ald_sector = .data$sector,
      ald_location = .data$asset_country,
      ald_production_unit = .data$unit,
      ald_emissions_factor = .data$emissions_factor,
      ald_emissions_factor_unit = .data$emissions_factor_unit
    ) %>%
    dplyr::mutate(
      id_name = "company_id",
      year = as.numeric(.data$year)
    )

  # ado 1182 - taken from data_preparation/convert_ald_to_2019_datastore_output.R
  masterdata_credit <- masterdata_credit %>%
    dplyr::mutate(
      technology = dplyr::case_when(
        .data$technology == "Oil and Condensate" ~ "Oil",
        .data$ald_sector == "Coal" ~ "Coal",
        TRUE ~ .data$technology
      )
    ) %>%
    dplyr::mutate(technology = sub("Grade$", "", .data$technology)) %>%
    dplyr::select(-.data$technology_type) %>%
    dplyr::group_by(dplyr::across(-.data$ald_production)) %>%
    dplyr::summarise(
      ald_production = sum(.data$ald_production, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup()

  masterdata_credit <- masterdata_credit %>%
    # left join, because there are companies w/o bbg_id that we want to keep
    dplyr::left_join(
      consolidated_financial_data %>%
        dplyr::select(.data$bloomberg_id, .data$country_of_domicile) %>%
        dplyr::filter(!is.na(.data$bloomberg_id)),
      by = c("id" = "bloomberg_id")
    )

  masterdata_credit <- masterdata_credit %>%
    dplyr::mutate(
      technology = dplyr::if_else(.data$ald_sector == "HDV", paste0(.data$technology, "_", .data$ald_sector), .data$technology),
      ald_sector = dplyr::if_else(.data$ald_sector == "HDV", "Automotive", .data$ald_sector)
    )

  # After this select, we have created the same format for credit masterdata as for the other types
  masterdata_credit <- masterdata_credit %>%
    dplyr::select(
      -c(
        .data$corporate_bond_ticker, .data$is_ultimate_parent,
        .data$is_ultimate_listed_parent, .data$company_status,
        .data$has_financial_data, .data$p_eu_eligible_gross,
        .data$p_eu_green_gross, .data$asset_level_timestamp,
        .data$number_of_assets, .data$metric, .data$bloomberg_id
      )
    )
}

#' Prepare raw input data files from Eikon
#'
#' @param data A list holding raw Eikon input data sets
#'
#' @return NULL
prepare_eikon_data_input <- function(data) {
  eikon_data <- dplyr::bind_rows(data)

  eikon_data <- eikon_data %>%
    dplyr::filter(.data$structural != "Implied Rating") %>%
    report_diff_rows(
      initial_n_rows = nrow(eikon_data),
      cause = "because of removing erroneously read in sub headers"
    )

  eikon_data <- eikon_data %>%
    dplyr::transmute(
      .data$isin,
      .data$structural,
      pd = as.double(.data$x4),
      profit_margin_preferred = .data$credit_smart_ratios_net_profit_margin_percent_ltm_s_avg,
      profit_margin_unpreferred = .data$net_profit_margin_percent_0d_ltm_1_s_avg,
      .data$leverage_s_avg,
      .data$asset_volatility_s_avg,
      .data$asset_drift_s_avg,
      .data$weighted_average_cost_of_capital_percent_s_avg,
      .data$ebitda_ltm_1_usd_s_avg,
      .data$ebitda_margin_percent_ltm_1_s_avg
    )
}

#' Prewrangle the input data files from Eikon
#'
#' @param data A data frame holding prepared Eikon input data sets
#' @param security_financial_data_input A data frame holding
#'   security_financial_data
#'
#' @return NULL
prewrangle_eikon_data <- function(data, security_financial_data_input) {
  # kickout rows with no ISIN -> low chance | much effort to match to our data
  eikon_data <- data %>%
    dplyr::filter(!is.na(.data$isin)) %>%
    report_diff_rows(
      initial_n_rows = nrow(data),
      cause = "because of NAs in the ISIN column"
    )

  # ensure isins are unique
  eikon_data <- eikon_data %>%
    dplyr::distinct(.data$isin, .keep_all = TRUE) %>%
    report_diff_rows(
      initial_n_rows = nrow(eikon_data),
      cause = "because of non unique ISINs"
    )

  # only filter ISINs which we have in our data and add company id
  security_financial_data_isin_id <- security_financial_data_input %>%
    dplyr::distinct(.data$isin, .data$company_id)

  eikon_data <- eikon_data %>%
    dplyr::inner_join(security_financial_data_isin_id, by = "isin") %>%
    report_diff_rows(
      initial_n_rows = nrow(eikon_data),
      cause = "because we inner joined our company ID by ISIN"
    ) %>%
    dplyr::select(-.data$isin)

  # drop (very few) companies without company_id
  eikon_data <- eikon_data %>%
    dplyr::filter(!is.na(.data$company_id)) %>%
    report_diff_rows(
      initial_n_rows = nrow(eikon_data),
      cause = "because company ID is missing"
    )

  # rename company_id to parent_company_id
  eikon_data <- eikon_data %>%
    dplyr::rename(parent_company_id = .data$company_id)
}

#' Prewrangle the ownership tree data from AR
#'
#' @param data A data frame holding the raw ownership tree data set
#' @return NULL
prewrangle_ownership_tree <- function(data) {
  # filter only one direction within ownership_tree
  ownership_tree <- data %>%
    dplyr::filter(.data$ownership_level >= 0) %>%
    report_diff_rows(
      initial_n_rows = nrow(data),
      cause = "because of the wrong direction in the ownership tree"
    )

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
    report_diff_rows(
      initial_n_rows = nrow(ownership_tree),
      cause = "by choosing only the majority parent"
    )
}

#' Identify which companies in the masterdata_ownership are missing in Eikon
#'
#' @param data A data frame holding the masterdata file in which to find the missings
#' @param company_identifier A data frame holding combinations of company_id,
#'   company_name and corporate_bond_ticker
#' @param eikon_data A data frame holding prepared Eikon data set
#'
#' @return NULL
find_missing_companies_ownership <- function(data, company_identifier, eikon_data) {
  # temporarily add company_name to eikon data to prep anti join
  eikon_data_company_name <- eikon_data %>%
    # ADO 1948 - no diff between left_join and inner_join, so I changed to the presumably safer one
    dplyr::inner_join(
      company_identifier,
      by = "company_id"
    )

  # take distinct company names
  # identify missing companies by keeping only companies in master data, that cannot be found in eikon
  # (bloomberg_id would be better but it somehow has NAs in the master data while company name does not)

  masterdata_ownership_missing_companies <- data %>%
    dplyr::distinct(.data$company_name) %>%
    dplyr::anti_join(eikon_data_company_name, by = "company_name") %>%
    report_diff_rows(
      initial_n_rows = nrow(data),
      cause = "because the other companies were already present in the Eikon Data"
    )

  # add company ID
  masterdata_ownership_missing_companies <- masterdata_ownership_missing_companies %>%
    dplyr::inner_join(
      company_identifier,
      by = "company_name"
    ) %>%
    # as number of rows increase --> company names are not unique because BBG
    # sometimes sees them as different (they could actually be)
    # no way to determine which company ID is the "right" one
    report_diff_rows(
      initial_n_rows = nrow(masterdata_ownership_missing_companies),
      cause = "by joining by company name"
    )

  # filter NAs in company ID
  masterdata_ownership_missing_companies <- masterdata_ownership_missing_companies %>%
    dplyr::select(.data$company_id) %>%
    dplyr::filter(!is.na(.data$company_id)) %>%
    report_diff_rows(
      initial_n_rows = nrow(masterdata_ownership_missing_companies),
      cause = "because company IDs are missing"
    )
}

#' Identify which companies in the masterdata_debt are missing in Eikon
#'
#' @param data A data frame holding the masterdata file in which to find the missings
#' @param company_identifier A data frame holding combinations of company_id,
#'   company_name and corporate_bond_ticker
#' @param eikon_data A data frame holding prepared Eikon data set
#'
#' @return NULL
find_missing_companies_debt <- function(data, company_identifier, eikon_data) {
  # take distinct corporate bond ticker
  masterdata_debt_missing_companies <- data %>%
    dplyr::distinct(.data$id)

  # filter missing IDs
  # take distinct corporate bond ticker
  masterdata_debt_missing_companies <- masterdata_debt_missing_companies %>%
    dplyr::filter(!is.na(.data$id)) %>%
    dplyr::inner_join(
      company_identifier,
      by = c("id" = "corporate_bond_ticker")
    ) %>%
    report_diff_rows(
      initial_n_rows = nrow(masterdata_debt_missing_companies),
      cause = "by adding all companies per coporate bond ticker"
    )

  # identify missing companies by corporate bond ticker
  masterdata_debt_missing_companies <- masterdata_debt_missing_companies %>%
    dplyr::anti_join(eikon_data, by = "company_id") %>%
    report_diff_rows(
      initial_n_rows = nrow(masterdata_debt_missing_companies),
      cause = "because the other companies were already present in the Eikon Data"
    )

  # filter NAs in company ID
  masterdata_debt_missing_companies <- masterdata_debt_missing_companies %>%
    dplyr::select(.data$company_id) %>%
    dplyr::filter(!is.na(.data$company_id)) %>%
    report_diff_rows(
      initial_n_rows = nrow(masterdata_debt_missing_companies),
      cause = "because company IDs are missing"
    )
}

#' Identify which companies in the masterdata_credit are missing in Eikon
#'
#' @param data A data frame holding the masterdata file in which to find the missings
#' @param eikon_data A data frame holding prepared Eikon data set
#'
#' @return NULL
find_missing_companies_credit <- function(data, eikon_data) {
  # take distinct company ids
  masterdata_credit_missing_companies <- data %>%
    dplyr::distinct(.data$id) %>%
    # ADO1948 - for loans, the id is the same as the company_id, so adding the company identifier is not necessary
    dplyr::rename(company_id = .data$id)

  # filter missing IDs
  masterdata_credit_missing_companies <- masterdata_credit_missing_companies %>%
    dplyr::filter(!is.na(.data$company_id))

  # identify missing companies by company_id
  masterdata_credit_missing_companies <- masterdata_credit_missing_companies %>%
    dplyr::anti_join(eikon_data, by = "company_id") %>%
    report_diff_rows(
      initial_n_rows = nrow(masterdata_credit_missing_companies),
      cause = "because the other companies were already present in the Eikon Data"
    )

  # filter NAs in company ID
  masterdata_credit_missing_companies <- masterdata_credit_missing_companies %>%
    dplyr::select(.data$company_id) %>%
    dplyr::filter(!is.na(.data$company_id)) %>%
    report_diff_rows(
      initial_n_rows = nrow(masterdata_credit_missing_companies),
      cause = "because company IDs are missing"
    )
}

#' Identify which companies in the masterdata_credit are missing in Eikon
#'
#' @param data A data frame holding prepared Eikon data set
#' @param missing_companies_ownership A data frame with missing companies from
#'   the masterdata_ownership data set
#' @param missing_companies_debt A data frame with missing companies from
#'   the masterdata_debt data set
#' @param missing_companies_credit A data frame with missing companies from
#'   the masterdata_credit data set
#'
#' @return NULL
add_missing_companies_to_eikon <- function(data,
                                           missing_companies_ownership,
                                           missing_companies_debt,
                                           missing_companies_credit) {
  # bind company IDs together
  masterdata_missing_companies <- rbind.data.frame(
    missing_companies_ownership,
    missing_companies_debt,
    missing_companies_credit
  )

  # ensure additional company IDs are unique (e.g. some corporate bond tickers
  # will also belong to companies in the ownership data)
  masterdata_missing_companies <- masterdata_missing_companies %>%
    dplyr::distinct(.data$company_id, .keep_all = TRUE) %>%
    report_diff_rows(
      initial_n_rows = nrow(masterdata_missing_companies),
      cause = "because of non unique company IDs"
    )

  # add to Eikon data
  eikon_data <- data %>%
    dplyr::bind_rows(
      masterdata_missing_companies
    ) %>%
    report_diff_rows(
      initial_n_rows = nrow(data),
      cause = "by adding the missing companies from AR"
    )
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

#' Removes companies from processed eikon data for which no asset based company
#' data can be found for any asset type.
#'
#' @param data A data frame holding processed Eikon data
#' @param md_ownership A data frame containing the masterdata_ownership data set
#' @param md_debt A data frame containing the masterdata_debt data set
#' @param md_credit A data frame containing the masterdata_credit data set
#'
#' @return A data frame
rm_companies_without_abcd <- function(data,
                                      md_ownership,
                                      md_debt,
                                      md_credit) {
  masterdata_ownership_without_na_company_name <- md_ownership %>%
    dplyr::filter(!is.na(.data$company_name)) %>%
    dplyr::distinct(.data$company_name) %>%
    dplyr::pull(.data$company_name)

  masterdata_debt_without_na_id <- md_debt %>%
    dplyr::filter(!is.na(.data$id)) %>%
    dplyr::distinct(.data$id) %>%
    dplyr::pull(.data$id)

  masterdata_credit_without_na_id <- md_credit %>%
    dplyr::filter(!is.na(.data$id)) %>%
    dplyr::distinct(.data$id) %>%
    dplyr::pull(.data$id)

  data <- data %>%
    dplyr::filter(
      .data$company_name %in% .env$masterdata_ownership_without_na_company_name |
        .data$corporate_bond_ticker %in% .env$masterdata_debt_without_na_id |
        .data$company_id %in% .env$masterdata_credit_without_na_id
    ) %>%
    report_diff_rows(
      initial_n_rows = nrow(data),
      cause = "by only filtering companies with ABCD"
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
      ratio_sufficient = dplyr::if_else(.data$ratio_sample_subgroup > .env$minimum_ratio_sample, TRUE, FALSE),
      avg_pd = stats::median(.data$pd, na.rm = TRUE),
      avg_structural = "NA",
      avg_profit_margin_preferred = stats::median(.data$profit_margin_preferred, na.rm = TRUE),
      avg_profit_margin_unpreferred = stats::median(.data$profit_margin_unpreferred, na.rm = TRUE),
      avg_leverage_s_avg = stats::median(.data$leverage_s_avg, na.rm = TRUE),
      avg_asset_volatility_s_avg = stats::median(.data$asset_volatility_s_avg, na.rm = TRUE),
      avg_asset_drift_s_avg = stats::median(.data$asset_drift_s_avg, na.rm = TRUE),
      avg_weighted_average_cost_of_capital_percent_s_avg = stats::median(.data$weighted_average_cost_of_capital_percent_s_avg, na.rm = TRUE),
      avg_ebitda_ltm_1_usd_s_avg = stats::median(.data$ebitda_ltm_1_usd_s_avg, na.rm = TRUE),
      avg_ebitda_margin_percent_ltm_1_s_avg = stats::median(.data$ebitda_margin_percent_ltm_1_s_avg, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      dplyr::across(dplyr::contains("avg_"), ~ (!is.na(.))),
      dplyr::across(dplyr::contains("avg_"), ~ (!is.infinite(.)))
    ) %>%
    dplyr::filter(.data$sample_sufficient == TRUE | .data$ratio_sufficient == TRUE) %>%
    dplyr::filter(
      dplyr::between(
        .data$avg_profit_margin_preferred,
        min(.env$allowed_range_npm, na.rm = TRUE),
        max(.env$allowed_range_npm, na.rm = TRUE)
      )
    )
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
      ggplot2::geom_point(ggplot2::aes(col = subregion), size = 4, alpha = 0.6) +
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
      final_indicator_type = paste("Financial indicator from", dplyr::if_else(!is.na(.data$eikon), "Eikon", paste(.data$average_type, "average"))),
      overall_data_type = paste(.data$source_company, .data$final_indicator_type, sep = " | ")
    )

  # order data types based on assumed accuracy
  eikon_data_long <- eikon_data_long %>%
    dplyr::mutate(
      overall_data_type = factor(
        .data$overall_data_type,
        levels = c(
          "Eikon company | Financial indicator from Eikon",
          "Subsidiary of eikon company | Financial indicator from Eikon",
          "Eikon company | Financial indicator from bics_subgroup_region average",
          "Eikon company | Financial indicator from security_mapped_sector_region average",
          "Eikon company | Financial indicator from bics_subgroup average",
          "Eikon company | Financial indicator from security_mapped_sector average",
          "Eikon company | Financial indicator from global average",
          "Subsidiary of eikon company | Financial indicator from bics_subgroup_region average",
          "Subsidiary of eikon company | Financial indicator from security_mapped_sector_region average",
          "Subsidiary of eikon company | Financial indicator from bics_subgroup average",
          "Subsidiary of eikon company | Financial indicator from security_mapped_sector average",
          "Subsidiary of eikon company | Financial indicator from global average",
          "AR company | Financial indicator from bics_subgroup_region average",
          "AR company | Financial indicator from security_mapped_sector_region average",
          "AR company | Financial indicator from bics_subgroup average",
          "AR company | Financial indicator from security_mapped_sector average",
          "AR company | Financial indicator from global average"
        )
      )
    )

  # pivot back to wide format
  data <- eikon_data_long %>%
    tidyr::pivot_wider(
      names_from = .data$name,
      names_sep = "_",
      values_from = c(.data$eikon, .data$avg, .data$final, .data$final_indicator_type, .data$overall_data_type)
    ) %>%
    assertr::verify(nrow(.) == nrow(data))

  # change variable classes back to doubles
  data <- data %>%
    dplyr::mutate(
      dplyr::across(
        c(dplyr::contains(c("final_", "eikon_", "avg_")), -dplyr::contains(c("_type_", "structural"))),
        ~ as.double(.)
      )
    )
}

#' For each company select the final financial value of the best granularity
#'
#' @param list_eikon_data A list of data frames containing the raw input data
#'   from eikon
#' @param security_financial_data A data frame containing the
#'   security_financial_data as prepared in the data_preparation repo
#' @param consolidated_financial_data A data frame containing the
#'   consolidated_financial_data as prepared in the data_preparation repo
#' @param ownership_tree A data frame containing the ownership_tree as provided
#'   by Asset Resolution
#' @param masterdata_ownership A data frame containing the masterdata_ownership
#'   as prepared in the data_preparation repo
#' @param masterdata_debt A data frame containing the masterdata_debt as
#'   prepared in the data_preparation repo
#' @param masterdata_credit A data frame containing the masterdata_credit as
#'   provided by Asset Resolution
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
prepare_eikon_data <- function(list_eikon_data,
                               security_financial_data,
                               consolidated_financial_data,
                               ownership_tree,
                               masterdata_ownership,
                               masterdata_debt,
                               masterdata_credit,
                               country_region_bridge,
                               n_min_sample,
                               min_ratio_sample_subgroup,
                               range_profit_margin) {
  # 1) Eikon preparation / pre wrangling----
  eikon_data_input <- prepare_eikon_data_input(list_eikon_data)


  prewrangled_eikon_data <- eikon_data_input %>%
    prewrangle_eikon_data(security_financial_data)

  # 2) Expand eikon data across ownership tree and bond_tickers-----

  # prepare ownership tree----
  prewrangled_ownership_tree <- prewrangle_ownership_tree(ownership_tree)

  # join in ownership tree----
  eikon_data <- prewrangled_eikon_data %>%
    # ADO 1948 - apparently no diff between left_join and inner_join, so changed to the more commonly correct one
    dplyr::inner_join(
      prewrangled_ownership_tree,
      by = c("parent_company_id" = "target_company_id")
    ) %>%
    report_diff_rows(
      initial_n_rows = nrow(prewrangled_eikon_data),
      cause = "by joining in the ownership tree"
    )

  rm(ownership_tree, prewrangled_ownership_tree)

  # ensure that company_ids are distinct----
  # ... but consider the most granular eikon
  # data points (e.g. take profit margin of child if available)

  # ensure that each company has a ownership level (slice_min kicks out NAs)
  eikon_data <- eikon_data %>%
    dplyr::filter(!is.na(.data$ownership_level)) %>%
    assertr::verify(nrow(.) == nrow(eikon_data))

  # take "closest" profit margins (i.e. own profit margin if available or from
  # the closest parent)
  eikon_data <- eikon_data %>%
    dplyr::group_by(.data$company_id) %>%
    dplyr::slice_min(.data$ownership_level) %>%
    dplyr::ungroup() %>%
    report_diff_rows(
      initial_n_rows = nrow(eikon_data),
      cause = "in order to only consider the closest information"
    )

  # 3) Add companies which have ALD but no eikon data (these will obtain averages)----

  # prep consolidated_financial_data for joins
  consolidated_financial_data_company_name_id_cb_ticker <- consolidated_financial_data %>%
    dplyr::distinct(.data$company_id, .data$company_name, .data$corporate_bond_ticker)

  # masterdata_ownership_missing_companies----
  masterdata_ownership_missing_companies <- masterdata_ownership %>%
    find_missing_companies_ownership(
      company_identifier = consolidated_financial_data_company_name_id_cb_ticker,
      eikon_data = eikon_data
    )

  # masterdata_debt_missing_companies----
  masterdata_debt_missing_companies <- masterdata_debt %>%
    find_missing_companies_debt(
      company_identifier = consolidated_financial_data_company_name_id_cb_ticker,
      eikon_data = eikon_data
    )

  # masterdata_credit_missing_companies----
  masterdata_credit_missing_companies <- masterdata_credit %>%
    find_missing_companies_credit(eikon_data)

  rm(consolidated_financial_data_company_name_id_cb_ticker)

  # add additional companies to eikon data----
  eikon_data <- eikon_data %>%
    add_missing_companies_to_eikon(
      missing_companies_ownership = masterdata_ownership_missing_companies,
      missing_companies_debt = masterdata_debt_missing_companies,
      missing_companies_credit = masterdata_credit_missing_companies
    )

  rm(
    masterdata_debt_missing_companies,
    masterdata_ownership_missing_companies,
    masterdata_credit_missing_companies
  )

  # for each company ID in the processed eikon data set, define the source of that
  # company (also used as an indicator later on)
  eikon_data <- eikon_data %>%
    add_company_source_info()

  # 4) add further company information------

  # add information such as bloomberg ids, corporate bonds tickers and company name
  eikon_data <- eikon_data %>%
    # ADO 1948 - left and inner behave the same, so I opt for the safer one
    dplyr::inner_join(
      consolidated_financial_data %>%
        dplyr::select(
          .data$bloomberg_id,
          .data$corporate_bond_ticker,
          .data$company_id,
          .data$company_name,
          .data$is_ultimate_listed_parent,
          .data$is_ultimate_parent,
          .data$market_cap,
          .data$country_of_domicile,
          .data$financial_sector,
          .data$bics_sector,
          .data$bics_subgroup
        ),
      by = "company_id"
    ) %>%
    assertr::verify(nrow(.) == nrow(eikon_data))

  # add security mapped sector (this determines the final sector in pacta and can
  # differ from financial sector)
  security_financial_data_sector_classifications <- security_financial_data %>%
    dplyr::filter(.data$security_mapped_sector != "Other") %>%
    dplyr::distinct(.data$company_id, .keep_all = TRUE) %>%
    dplyr::select(
      .data$company_id,
      .data$security_mapped_sector
    )

  eikon_data <- eikon_data %>%
    dplyr::left_join(
      security_financial_data_sector_classifications,
      by = "company_id"
    ) %>%
    dplyr::mutate(
      security_mapped_sector = dplyr::if_else(
        is.na(.data$security_mapped_sector),
        "Other",
        .data$security_mapped_sector
      )
    ) %>%
    assertr::verify(nrow(.) == nrow(eikon_data))

  # join in regional bridge for country_of_domicile
  eikon_data <- eikon_data %>%
    dplyr::left_join(
      country_region_bridge,
      by = c("country_of_domicile" = "iso_a2")
    ) %>%
    assertr::verify(nrow(.) == nrow(eikon_data))

  # arrange columns
  eikon_data <- eikon_data %>%
    dplyr::select(
      .data$company_name,
      .data$country_of_domicile,
      .data$financial_sector,
      .data$security_mapped_sector,
      .data$bics_sector,
      .data$bics_subgroup,
      .data$company_id,
      .data$bloomberg_id,
      .data$corporate_bond_ticker,
      .data$is_ultimate_listed_parent,
      .data$is_ultimate_parent,
      .data$market_cap,
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
    report_diff_rows(
      initial_n_rows = nrow(eikon_data),
      cause = "to ensure unique company names"
    )

  # only filter companies with ALD --> kick out out irrelevant eikon companies or
  # irrelevant added subsidiaries
  eikon_data <- eikon_data %>%
    rm_companies_without_abcd(
      md_ownership = masterdata_ownership,
      md_debt = masterdata_debt,
      md_credit = masterdata_credit
    )

  # 5) create averages by different granularities of sub groups----
  # (but only based on values we obtained directly from eikon)

  # create averages by bics subsector and region
  bics_subgroup_region_averages <- eikon_data %>%
    dplyr::filter(!is.na(.data$bics_subgroup), !is.na(.data$subregion)) %>%
    dplyr::group_by(.data$bics_subgroup, .data$subregion) %>%
    create_averages_eikon(
      minimum_sample_size = n_min_sample,
      minimum_ratio_sample = min_ratio_sample_subgroup,
      allowed_range_npm = range_profit_margin
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(average_type = "bics_subgroup_region")

  # create averages by security_mapped_sector and region
  security_mapped_sector_region_averages <- eikon_data %>%
    dplyr::filter(!is.na(.data$security_mapped_sector), !is.na(.data$subregion)) %>%
    dplyr::group_by(.data$security_mapped_sector, .data$subregion) %>%
    create_averages_eikon(
      minimum_sample_size = n_min_sample,
      minimum_ratio_sample = min_ratio_sample_subgroup,
      allowed_range_npm = range_profit_margin
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(average_type = "security_mapped_sector_region")

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

  # create global security_mapped_sector average
  security_mapped_sector_averages <- eikon_data %>%
    dplyr::filter(!is.na(.data$security_mapped_sector)) %>%
    dplyr::group_by(.data$security_mapped_sector) %>%
    create_averages_eikon(
      minimum_sample_size = n_min_sample,
      minimum_ratio_sample = min_ratio_sample_subgroup,
      allowed_range_npm = range_profit_margin
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(average_type = "security_mapped_sector")

  # create global average
  global_averages <- eikon_data %>%
    create_averages_eikon(
      minimum_sample_size = n_min_sample,
      minimum_ratio_sample = min_ratio_sample_subgroup,
      allowed_range_npm = range_profit_margin
    ) %>%
    dplyr::mutate(average_type = "global")


  # create plots for sub group averages----
  plot_bics_subgroup_region_averages <- bics_subgroup_region_averages %>%
    # dplyr::filter(dplyr::between(.data$avg_profit_margin_preferred, -2,2)) %>%
    plot_sector_averages(
      x = "bics_subgroup",
      y = "avg_profit_margin_preferred",
      subregion = TRUE
    )

  plot_security_mapped_sector_region_averages <- security_mapped_sector_region_averages %>%
    # dplyr::filter(dplyr::between(.data$avg_profit_margin_preferred, -2,2)) %>%
    plot_sector_averages(
      x = "security_mapped_sector",
      y = "avg_profit_margin_preferred",
      subregion = TRUE
    )

  plot_bics_subgroup_averages <- bics_subgroup_averages %>%
    dplyr::filter(dplyr::between(.data$avg_profit_margin_preferred, -2, 2)) %>%
    plot_sector_averages(
      x = "bics_subgroup",
      y = "avg_profit_margin_preferred"
    )

  plot_security_mapped_sector_averages <- security_mapped_sector_averages %>%
    # dplyr::filter(dplyr::between(.data$avg_profit_margin_preferred, -2,2)) %>%
    plot_sector_averages(
      x = "security_mapped_sector",
      y = "avg_profit_margin_preferred"
    )

  # add most the most granular average calculated to the eikon data----
  # subset companies for which we have good bics_subgroup + regional averages available
  eikon_data_bics_subgroup_region_averages <- eikon_data %>%
    dplyr::inner_join(bics_subgroup_region_averages, by = c("bics_subgroup", "subregion"))

  # subset companies for which we have good security_mapped_sector + regional averages available
  eikon_data_security_mapped_sector_region_averages <- eikon_data %>%
    dplyr::anti_join(eikon_data_bics_subgroup_region_averages, by = "company_id") %>%
    dplyr::inner_join(security_mapped_sector_region_averages, by = c("security_mapped_sector", "subregion"))

  # subset companies for which we have good bics_subgroup averages available + which havent subset beforehand
  eikon_data_bics_subgroup_averages <- eikon_data %>%
    dplyr::anti_join(eikon_data_bics_subgroup_region_averages, by = "company_id") %>%
    dplyr::anti_join(eikon_data_security_mapped_sector_region_averages, by = "company_id") %>%
    dplyr::inner_join(bics_subgroup_averages, by = "bics_subgroup")

  # subset companies for which we have good security_mapped_sector averages available + which havent subset beforehand
  eikon_data_security_mapped_sector_averages <- eikon_data %>%
    dplyr::anti_join(eikon_data_bics_subgroup_region_averages, by = "company_id") %>%
    dplyr::anti_join(eikon_data_security_mapped_sector_region_averages, by = "company_id") %>%
    dplyr::anti_join(eikon_data_bics_subgroup_averages, by = "company_id") %>%
    dplyr::inner_join(security_mapped_sector_averages, by = "security_mapped_sector")

  # add global averages for companies which havent obtained averages beforehand
  eikon_data_global_averages <- eikon_data %>%
    dplyr::anti_join(eikon_data_bics_subgroup_region_averages, by = "company_id") %>%
    dplyr::anti_join(eikon_data_security_mapped_sector_region_averages, by = "company_id") %>%
    dplyr::anti_join(eikon_data_bics_subgroup_averages, by = "company_id") %>%
    dplyr::anti_join(eikon_data_security_mapped_sector_averages, by = "company_id") %>%
    dplyr::bind_cols(global_averages)

  # bind together
  eikon_data <- rbind.data.frame(
    eikon_data_bics_subgroup_region_averages,
    eikon_data_security_mapped_sector_region_averages,
    eikon_data_bics_subgroup_averages,
    eikon_data_security_mapped_sector_averages,
    eikon_data_global_averages
  ) %>%
    assertr::verify(nrow(.) == nrow(eikon_data))

  rm(
    n_min_sample, min_ratio_sample_subgroup, range_profit_margin,
    eikon_data_bics_subgroup_region_averages, bics_subgroup_region_averages,
    bics_subgroup_averages, security_mapped_sector_region_averages,
    security_mapped_sector_averages, eikon_data_bics_subgroup_averages,
    eikon_data_global_averages, eikon_data_security_mapped_sector_region_averages,
    eikon_data_security_mapped_sector_averages, global_averages
  )

  # create new avg rating based on avg PDs. Rating boundaries based on input data
  # TODO: ADO 3542 - this currently throws warnings. we do not use the variable
  # though so we can fix it later
  for (e in unique(eikon_data$structural)) {
    eikon_data <- eikon_data %>%
      dplyr::mutate(
        avg_structural =
          dplyr::case_when(
            dplyr::between(
              avg_pd,
              eikon_data %>% dplyr::filter(structural == e) %>% dplyr::summarise(min(pd, na.rm = TRUE)),
              eikon_data %>% dplyr::filter(structural == e) %>% dplyr::summarise(max(pd, na.rm = TRUE))
            ) & avg_structural == "NA" ~ e,
            TRUE ~ avg_structural
          )
      )

    rm(e)
  }

  # ensure that no NAs and Inf are in the avg data
  eikon_data %>%
    dplyr::filter(dplyr::if_any(dplyr::contains("avg_"), ~ (is.na(.))) | dplyr::if_any(dplyr::contains("avg_"), ~ (is.infinite(.)))) %>%
    assertr::verify(nrow(.) == 0)

  # 6) pick the best financial data point available----
  eikon_data <- eikon_data %>%
    select_final_financial_value()
}

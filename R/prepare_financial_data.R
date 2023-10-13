library(dplyr)

add_column_company_id_to_eikon_data <- function(eikon_data, ids_data) {
  financial_data <- eikon_data %>%
    dplyr::inner_join(ids_data %>% dplyr::distinct(isin, company_id), by = dplyr::join_by(isin))

  return(financial_data)
}


add_column_ald_sector_to_financial_data <- function(financial_data, companies_data) {
  financial_data <- financial_data %>%
    dplyr::left_join(companies_data %>% dplyr::distinct(company_id, ald_sector),
      by = dplyr::join_by(company_id),
      relationship = "many-to-many"
    )

  return(financial_data)
}

match_location_to_region <- function(financial_data) {
  # country region bridge-------
  country_region_bridge <- countrycode::codelist %>%
    dplyr::filter(!is.na(.data$ecb)) %>%
    dplyr::distinct(.data$region, .data$ecb) %>%
    dplyr::rename(ald_location = .data$ecb)

  # extract location from ISIN (iso country code is the first 2 characters)
  financial_data <- financial_data %>%
    dplyr::left_join(country_region_bridge, by = dplyr::join_by(ald_location))

  return(financial_data)
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
  subgroup_averages <- data %>%
    dplyr::mutate(size_subgroup = dplyr::n()) %>%
    dplyr::summarise(
      size_subgroup = mean(.data$size_subgroup, na.rm = TRUE),
      size_sample = dplyr::n(),
      ratio_sample_subgroup = .data$size_sample / .data$size_subgroup,
      sample_sufficient = dplyr::if_else(.data$size_sample > .env$minimum_sample_size, TRUE, FALSE),
      ratio_sufficient = dplyr::if_else(.data$ratio_sample_subgroup > .env$minimum_ratio_sample, TRUE, FALSE),
      avg_pd = stats::median(.data$pd, na.rm = TRUE),
      avg_structural = "NA",
      avg_ticker_symbol = "NA",
      avg_net_profit_margin = stats::median(.data$net_profit_margin, na.rm = TRUE),
      avg_debt_equity_ratio = stats::median(.data$debt_equity_ratio, na.rm = TRUE),
      avg_volatility = stats::median(.data$volatility, na.rm = TRUE),
      avg_asset_drift = stats::median(.data$asset_drift, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(
      dplyr::across(dplyr::contains("avg_"), ~ (!is.na(.))),
      dplyr::across(dplyr::contains("avg_"), ~ (!is.infinite(.)))
    ) %>%
    dplyr::filter(.data$sample_sufficient == TRUE | .data$ratio_sufficient == TRUE) %>%
    dplyr::filter(
      dplyr::between(
        .data$avg_net_profit_margin,
        min(.env$allowed_range_npm, na.rm = TRUE),
        max(.env$allowed_range_npm, na.rm = TRUE)
      )
    )
  return(subgroup_averages)
}

aggregate_financial_data_to_company_level <- function(financial_data) {
  financial_data <- financial_data %>%
    dplyr::filter(!is.na(.data$company_id)) %>%
    dplyr::group_by(.data$company_id) %>%
    create_averages_eikon(
      minimum_sample_size = 0,
      minimum_ratio_sample = 0,
      allowed_range_npm = c(-Inf, Inf)
    ) %>%
    dplyr::select(
      .data$company_id, .data$avg_pd, .data$avg_structural,
      .data$avg_ticker_symbol, .data$avg_net_profit_margin,
      .data$avg_debt_equity_ratio, .data$avg_volatility, .data$avg_asset_drift
    ) %>%
    dplyr::rename(
      pd = .data$avg_pd, 
      structural = .data$avg_structural,
      ticker_symbol = .data$avg_ticker_symbol,
      net_profit_margin = .data$avg_net_profit_margin,
      debt_equity_ratio = .data$avg_debt_equity_ratio,
      volatility = .data$avg_volatility,
      asset_drift = .data$avg_asset_drift
    )
}

expand_financial_data_across_ownership_tree <- function(financial_data, ownership_tree){
  # join in ownership tree----
  eikon_data <- financial_data %>%
    dplyr::left_join(
      ownership_tree,
      by = dplyr::join_by(company_id)
    ) 
    # %>%
    # report_diff_rows(
    #   initial_n_rows = nrow(prewrangled_eikon_data),
    #   cause = "by joining in the ownership tree"
    # )

  # ensure that company_ids are distinct----
  # ... but consider the most granular eikon
  # data points (e.g. take profit margin of child if available)

  # ensure that each company has a ownership level (slice_min kicks out NAs)
  eikon_data <- eikon_data %>%
    dplyr::mutate(ownership_level=dplyr::if_else(is.na(.data$parent_company_id), 0, .data$ownership_level)) %>%
    assertr::verify(nrow(.) == nrow(eikon_data))

  # take "closest" profit margins (i.e. own profit margin if available or from
  # the closest parent)
  eikon_data <- eikon_data %>%
    dplyr::group_by(.data$company_id) %>%
    dplyr::slice_min(.data$ownership_level) %>%
    dplyr::ungroup() 
    # %>%
    # report_diff_rows(
    #   initial_n_rows = nrow(eikon_data),
    #   cause = "in order to only consider the closest information"
    # )
  eikon_data  %>% 
    dplyr::select(-c(.data$parent_company_id, .data$linking_stake, .data$ownership_level)) %>% 
    assertr::verify(nrow(.) == nrow(financial_data))
  return(eikon_data)
}

add_missing_companies_to_financial_data <- function(financial_data, companies_data) {
  companies_with_region <- companies_data %>% distinct(company_id, ald_sector, ald_location)
  financial_data <-  dplyr::bind_rows(
      financial_data,
      companies_with_region %>% dplyr::anti_join(financial_data, by=dplyr::join_by(company_id))
      )
  return(financial_data)
}

#' For each company select the final financial value of the best granularity
#'
#' @param data A data frame which contains the financial data including the
#'   calculated averages for a combination of sectors and regions
#'
#' @return A data frame
select_final_financial_value_using_averages <- function(data) {
  # pivot long company info + indicators from eikon
  eikon_values_long <- data %>%
    dplyr::select(-names(data)[stringr::str_detect(names(data), "avg_")]) %>%
    tidyr::pivot_longer(
      cols = stringr::str_remove_all(names(data)[stringr::str_detect(names(data), "avg_")], "avg_"),
      values_to = "eikon",
      values_transform = list(eikon = as.character) # TODO Why is this here ?
    ) %>%
    dplyr::mutate(eikon = as.double(.data$eikon))

  # pivot long averages
  average_values_long <- data %>%
    dplyr::select(.data$company_id, names(data)[stringr::str_detect(names(data), "avg_")]) %>%
    tidyr::pivot_longer(
      cols = !"company_id",
      values_to = "avg",
      values_transform = list(avg = as.character)
    ) %>%
    dplyr::mutate(
      name = stringr::str_remove_all(.data$name, "avg_"),
      avg = as.double(.data$avg)
    )

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
      overall_data_type = paste(.data$final_indicator_type, sep = " | ")
    )

  # order data types based on assumed accuracy
  eikon_data_long <- eikon_data_long %>%
    dplyr::mutate(
      overall_data_type = factor(
        .data$overall_data_type,
        levels = c(
          "Financial indicator from Eikon",
          "Financial indicator from bics_subgroup_region average",
          "Eikon company | Financial indicator from bics_subgroup average",
          "Eikon company | Financial indicator from global average"
        )
      )
    )

  # pivot back to wide format
  data <- eikon_data_long %>%
    tidyr::pivot_wider(
      id_cols=c("isin", "company_id", "ald_sector", "ald_location"),
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
  data
}


add_columns_financial_averages <- function(
    financial_data,
    minimum_sample_size,
    minimum_ratio_sample,
    allowed_range_npm) {
  # 5) create averages by different granularities of sub groups----
  # (but only based on values we obtained directly from eikon)

  # create averages by bics subsector and region
  ald_sector_region_averages <- financial_data %>%
    dplyr::filter(!is.na(.data$ald_sector), !is.na(.data$region)) %>%
    dplyr::group_by(.data$ald_sector, .data$region) %>%
    create_averages_eikon(
      minimum_sample_size,
      minimum_ratio_sample,
      allowed_range_npm
    ) %>%
    dplyr::mutate(average_type = "bics_subgroup_region")

  # create global bics_subgroup average
  ald_sector_averages <- financial_data %>%
    dplyr::filter(!is.na(.data$ald_sector)) %>%
    dplyr::group_by(.data$ald_sector) %>%
    create_averages_eikon(
      minimum_sample_size,
      minimum_ratio_sample,
      allowed_range_npm
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(average_type = "bics_subgroup")

  # create global average
  global_averages <- financial_data %>%
    create_averages_eikon(
      minimum_sample_size ,
      minimum_ratio_sample ,
      allowed_range_npm 
    ) %>%
    dplyr::mutate(average_type = "global")

  # add most the most granular average calculated to the eikon data----
  # subset companies for which we have good bics_subgroup + regional averages available
  financial_data_ald_sector_region_averages <- financial_data %>%
    dplyr::inner_join(ald_sector_region_averages, by = c("ald_sector", "region"))

  # subset companies for which we have good bics_subgroup averages available + which havent subset beforehand
  financial_data_ald_sector_averages <- financial_data %>%
    dplyr::anti_join(financial_data_ald_sector_region_averages, by = c("company_id","ald_sector", "region")) %>%
    dplyr::inner_join(ald_sector_averages, by = "ald_sector")

  # add global averages for companies which havent obtained averages beforehand
  financial_data_global_averages <- financial_data %>%
    dplyr::anti_join(financial_data_ald_sector_region_averages, by = c("company_id","ald_sector", "region")) %>%
    dplyr::anti_join(financial_data_ald_sector_averages, by = c( "ald_sector", "company_id") )%>%
    dplyr::bind_cols(global_averages)

  # bind together
  financial_data <- rbind.data.frame(
    financial_data_ald_sector_region_averages,
    financial_data_ald_sector_averages,
    financial_data_global_averages
  ) %>%
    assertr::verify(nrow(.) == nrow(financial_data))


  # ensure that no NAs and Inf are in the avg data
  financial_data %>%
    dplyr::filter(dplyr::if_any(dplyr::contains("avg_"), ~ (is.na(.))) | dplyr::if_any(dplyr::contains("avg_"), ~ (is.infinite(.)))) %>%
    assertr::verify(nrow(.) == 0)

  return(financial_data)
}


prepare_financial_data <- function() {
  output_dir <- fs::path("data-raw", "DBs")

  eikon_data <- readr::read_rds(fs::path(output_dir, "DB_assets_eikon.rds"))
  companies_data <- readr::read_rds(fs::path(output_dir, "DB_asset_impact.rds"))
  ids_data <- readr::read_rds(fs::path(output_dir, "DB_ids.rds"))
  ownership_tree <- readr::read_rds(fs::path(output_dir, "DB_ownership_tree.rds"))

  financial_data <- add_column_company_id_to_eikon_data(eikon_data, ids_data)
  financial_data <- add_column_ald_sector_to_financial_data(financial_data, companies_data)
   # filter rows without a company ID as they're not useful anymore 
   # maybe one day they will for missing values averages filling
  financial_data <- financial_data %>% dplyr::filter(!is.na(company_id))

  financial_data <- expand_financial_data_across_ownership_tree(financial_data, ownership_tree)
  financial_data <- add_missing_companies_to_financial_data(financial_data, companies_data)
  financial_data <- match_location_to_region(financial_data)

  # parameters for minimum requirements to reference subgroups in creating averages
  # determine size of subgroup below which we do not use the average because the
  # minimum required sample size of reference subgroup
  minimum_sample_size <- 50
  # minimum required ratio of reference subgroup to sample
  minimum_ratio_sample <- 1 / 3
  # cut off values for profit margins
  allowed_range_npm <- c(-Inf, Inf)

  financial_data <- add_columns_financial_averages(
    financial_data,
    minimum_sample_size,
    minimum_ratio_sample, 
    allowed_range_npm)
  financial_data <- select_final_financial_value_using_averages(financial_data)
  
  financial_data <- aggregate_financial_data_to_company_level(financial_data)
}

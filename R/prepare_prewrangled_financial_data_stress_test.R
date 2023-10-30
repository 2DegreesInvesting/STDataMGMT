#' Map the columns (company_id, ald_sector) from companies data to the financial data
#' where applicable (through a left join)
#'
#' @param financial_data financial_data
#' @param companies_data companies_data
#'
#' @return a dataframe
#'
add_column_ald_sector_to_financial_data <- function(financial_data, companies_data) {
  financial_data <- financial_data %>%
    dplyr::left_join(companies_data %>% dplyr::distinct(.data$company_id, .data$ald_sector),
      by = c("company_id"),
      relationship = "many-to-many"
    )

  return(financial_data)
}

#' Decrease regional granularity by matching
#' the ald_location countries in financial data to macro regions
#'
#' @param financial_data financial_data
#'
#' @return a dataframe
#'
match_location_to_region <- function(financial_data) {
  # country region bridge-------
  country_region_bridge <- countrycode::codelist %>%
    dplyr::filter(!is.na(.data$ecb)) %>%
    dplyr::distinct(.data$region, .data$ecb) %>%
    dplyr::rename(
      ald_location = .data$ecb,
      ald_region = .data$region
    )

  # extract location from ISIN (iso country code is the first 2 characters)
  financial_data <- financial_data %>%
    dplyr::left_join(country_region_bridge, by = c("ald_location")) %>%
    dplyr::select(-c(.data$ald_location))

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
    ) %>%
    dplyr::select(-c(.data$size_subgroup, .data$size_sample, .data$ratio_sample_subgroup, .data$sample_sufficient, .data$ratio_sufficient))

  # # OTHER WAY TO DO IT.
  # # group duplicate company_id rows,
  # # taking average of non-NA numeric value, any(first non-NA) character value
  # eikon_data <- eikon_data %>%
  #   dplyr::group_by(.data$company_id) %>%
  #   dplyr::summarise(dplyr::across(
  #     dplyr::everything(),
  #     ~ ifelse(
  #       is.numeric(.x),
  #       stats::median(na.omit(.x)),
  #       dplyr::first(na.omit(.x))
  #     )
  #   ))

  return(subgroup_averages)
}


#' Applies the create_averages_eikon() function with minimum tolerance, effectively
#' aggregating all financial indicators on the grp_cols columns
#'
#' @param financial_data financial_data
#' @param grp_cols grp_cols
#'
#' @return a dataframe
#'
aggregate_financial_indicators <- function(financial_data, grp_cols) {
  financial_data <- financial_data %>%
    dplyr::group_by(dplyr::across(grp_cols)) %>%
    create_averages_eikon(
      minimum_sample_size = 0,
      minimum_ratio_sample = 0,
      allowed_range_npm = c(-Inf, Inf)
    ) %>%
    dplyr::rename(
      pd = .data$avg_pd,
      net_profit_margin = .data$avg_net_profit_margin,
      debt_equity_ratio = .data$avg_debt_equity_ratio,
      volatility = .data$avg_volatility,
      asset_drift = .data$avg_asset_drift
    )
}

#' Aggregate financial data indicator types using dummies.
#' This does NOT return dummies as boolean, but as the sum of all indicator
#' types when grouped by company_id.
#'
#' @param financial_data financial_data
#' @param grp_cols grp_cols
#'
#' @return a dataframe
#'
aggregate_indicator_types <- function(financial_data, grp_cols) {
  indicator_type_cols <- names(financial_data)[grepl("indicator_type_", names(financial_data))]
  financial_data_indicator_types <- financial_data %>% dplyr::select(c("company_id", indicator_type_cols))
  
  financial_data_indicator_types[indicator_type_cols] <- lapply(
    financial_data_indicator_types[indicator_type_cols],
    function(x) {
      stringr::str_remove(x, "Financial indicator from ")
    }
  )

  aggregated_financial_data_indicator_types <- financial_data_indicator_types %>%
    fastDummies::dummy_cols(select_columns = indicator_type_cols, remove_selected_columns = TRUE) %>%
    dplyr::group_by(dplyr::across(grp_cols)) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), sum), .groups = "drop")

  return(aggregated_financial_data_indicator_types)
}

#' Use the ownership tree to assign raw eikon financial data to a subsidiary company when it
#' doesn't own any financial information in the raw data
#'
#' @param missing_companies_in_financial_data missing_companies_in_financial_data
#' @param financial_data financial_data
#' @param ownership_tree ownership_tree
#'
#' @return a dataframe
#'
match_closest_financial_data_to_missing_companies <- function(missing_companies_in_financial_data, financial_data, ownership_tree) {
  # join in ownership tree----
  missing_companies_in_financial_data <- missing_companies_in_financial_data %>%
    dplyr::left_join(
      ownership_tree,
      by = c("company_id" = "subsidiary_company_id"),
      relationship = "many-to-many"
    )

  # ensure that company_ids are distinct----
  # ... but consider the most granular eikon
  # data points (e.g. take profit margin of child if available)

  # ensure that each company has a ownership level (slice_min kicks out NAs)
  # and a company_id (in case the parent_company_id doesn't exist in the ownership tree)
  missing_companies_in_financial_data <- missing_companies_in_financial_data %>%
    dplyr::mutate(
      ownership_level = dplyr::if_else(is.na(.data$ownership_level), 0, .data$ownership_level),
      parent_company_id = dplyr::if_else(is.na(.data$parent_company_id), .data$company_id, .data$parent_company_id)
    )

  # filter out rows to keep the closest parent company
  missing_companies_in_financial_data <- missing_companies_in_financial_data %>%
    dplyr::group_by(.data$company_id, .data$ald_sector, .data$ald_region) %>%
    dplyr::slice_min(.data$ownership_level) %>%
    dplyr::ungroup()

  missing_companies_in_financial_data <- missing_companies_in_financial_data %>%
    dplyr::left_join(financial_data,
      by = c(
        "parent_company_id" = "company_id",
        "ald_region" = "ald_region",
        "ald_sector" = "ald_sector"
      )
    )

  missing_companies_in_financial_data <- missing_companies_in_financial_data %>%
    dplyr::select(-c(.data$parent_company_id, .data$linking_stake, .data$ownership_level))

  # check that there are no duplicates
  missing_companies_in_financial_data %>%
    dplyr::distinct(.data$company_id, .data$ald_sector, .data$ald_region) %>%
    assertr::verify(nrow(.) == nrow(missing_companies_in_financial_data))

  return(missing_companies_in_financial_data)
}

#' Anti join the financial data with companies data to create new rows
#' in the financial data for the missing companies.
#'
#' @param financial_data financial_data
#' @param companies_data companies_data
#'
#' @return a dataframe
#'
get_missing_companies_in_financial_data <- function(financial_data, companies_data) {
  missing_companies_in_financial_data <- companies_data %>%
    dplyr::distinct(.data$company_id, .data$ald_sector, .data$ald_location) %>%
    dplyr::anti_join(financial_data, by = "company_id")
  return(missing_companies_in_financial_data)
}


#' compute averages on sector/region , sector, global, using the
#' create_averages_eikon() function
#' TODO global/region
#'
#' @param financial_data financial_data
#' @param minimum_sample_size minimum_sample_size
#' @param minimum_ratio_sample minimum_ratio_sample
#' @param allowed_range_npm allowed_range_npm
#'
#' @return a list of dataframes
#'
compute_financial_averages <- function(
    financial_data,
    minimum_sample_size,
    minimum_ratio_sample,
    allowed_range_npm) {
  # 5) create averages by different granularities of sub groups----
  # (but only based on values we obtained directly from eikon)

  # create averages by bics subsector and region
  ald_sector_region_averages <- financial_data %>%
    dplyr::filter(!is.na(.data$ald_sector), !is.na(.data$ald_region)) %>%
    dplyr::group_by(.data$ald_sector, .data$ald_region) %>%
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
      minimum_sample_size,
      minimum_ratio_sample,
      allowed_range_npm
    ) %>%
    dplyr::mutate(average_type = "global")

  return(list(
    ald_sector_region_averages = ald_sector_region_averages,
    ald_sector_averages = ald_sector_averages,
    global_averages = global_averages
  ))
}

#' Add financial averages columns to the data in order, f
#' rom most granular to less granular
#'
#' @param financial_data financial_data
#' @param ald_sector_region_averages ald_sector_region_averages
#' @param ald_sector_averages ald_sector_averages
#' @param global_averages global_averages
#'
#' @return a dataframe
#'
add_columns_financial_averages <- function(
    financial_data,
    ald_sector_region_averages,
    ald_sector_averages,
    global_averages) {
  # add most the most granular average calculated to the eikon data----
  # subset companies for which we have good bics_subgroup + regional averages available
  financial_data_ald_sector_region_averages <- financial_data %>%
    dplyr::inner_join(ald_sector_region_averages, by = c("ald_sector", "ald_region"))

  # subset companies for which we have good bics_subgroup averages available + which havent subset beforehand
  financial_data_ald_sector_averages <- financial_data %>%
    dplyr::anti_join(financial_data_ald_sector_region_averages, by = c("company_id", "ald_sector", "ald_region")) %>%
    dplyr::inner_join(ald_sector_averages, by = "ald_sector")

  # add global averages for companies which havent obtained averages beforehand
  financial_data_global_averages <- financial_data %>%
    dplyr::anti_join(financial_data_ald_sector_region_averages, by = c("company_id", "ald_sector", "ald_region")) %>%
    dplyr::anti_join(financial_data_ald_sector_averages, by = c("ald_sector", "company_id")) %>%
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


#' For each company select the final financial value of the best granularity
#'
#' @param financial_data A data frame which contains the financial data including the
#'   calculated averages for a combination of sectors and regions
#'
#' @return A data frame
select_final_financial_value_using_averages <- function(financial_data) {
  # pivot long company info + indicators from eikon
  eikon_values_long <- financial_data %>%
    dplyr::select(-names(financial_data)[stringr::str_detect(names(financial_data), "avg_")]) %>%
    tidyr::pivot_longer(
      cols = stringr::str_remove_all(names(financial_data)[stringr::str_detect(names(financial_data), "avg_")], "avg_"),
      values_to = "eikon",
      values_transform = list(eikon = as.character) # TODO Why is this here ?
    ) %>%
    dplyr::mutate(eikon = as.double(.data$eikon))

  # pivot long averages
  average_values_long <- financial_data %>%
    dplyr::select(.data$company_id, .data$ald_region, .data$ald_sector, names(financial_data)[stringr::str_detect(names(financial_data), "avg_")]) %>%
    tidyr::pivot_longer(
      cols = !c("company_id", "ald_region", "ald_sector"),
      values_to = "avg",
      values_transform = list(avg = as.character)
    ) %>%
    dplyr::mutate(
      name = stringr::str_remove_all(.data$name, "avg_"),
      avg = as.double(.data$avg)
    )

  # join both long formats
  # in eikon_values_long are the original financial values, can be NA
  # in average_values_long are the computed averages, that will replace NA values in the _eikon column
  eikon_data_long <- eikon_values_long %>%
    dplyr::left_join(
      average_values_long,
      by = c("name", "company_id", "ald_region", "ald_sector"),
      suffix = c("_eikon", "_avg") # TODO remove has no effect
    )

  # choose final indicators + determine data types
  eikon_data_long <- eikon_data_long %>%
    dplyr::mutate(
      final = dplyr::if_else(!is.na(.data$eikon), .data$eikon, .data$avg),
      final_indicator_type = paste("Financial indicator from", dplyr::if_else(!is.na(.data$eikon), "Eikon", paste(.data$average_type, "average"))),
      overall_data_type = paste(.data$final_indicator_type, sep = " | ")
    )

  # order data types based on assumed accuracy
  # TODO remove has no effect
  eikon_data_long <- eikon_data_long %>%
    dplyr::mutate(
      overall_data_type = factor(
        .data$overall_data_type,
        levels = c(
          "Financial indicator from Eikon",
          "Financial indicator from bics_subgroup_region average",
          "Financial indicator from bics_subgroup average",
          "Financial indicator from global average"
        )
      )
    )

  # pivot back to wide format
  financial_data <- eikon_data_long %>%
    tidyr::pivot_wider(
      # id_cols=c("company_id", "ald_sector", "ald_region"),
      names_from = .data$name,
      names_sep = "_",
      values_from = c(.data$eikon, .data$avg, .data$final, .data$final_indicator_type, .data$overall_data_type)
    ) %>%
    assertr::verify(nrow(.) == nrow(financial_data))

  # change variable classes back to doubles
  financial_data <- financial_data %>%
    dplyr::mutate(
      dplyr::across(
        c(dplyr::contains(c("final_", "eikon_", "avg_")), -dplyr::contains(c("_type_", "structural"))),
        ~ as.double(.)
      )
    )
  # select final columns and clean column names
  financial_data <- financial_data %>%
    dplyr::select(
      .data$company_id, .data$ald_region, .data$ald_sector,
      .data$final_pd,
      .data$final_net_profit_margin,
      .data$final_debt_equity_ratio,
      .data$final_volatility,
      .data$final_asset_drift,
      .data$final_indicator_type_net_profit_margin,
      .data$final_indicator_type_debt_equity_ratio,
      .data$final_indicator_type_volatility,
      .data$final_indicator_type_asset_drift
    )

  names(financial_data) <- names(financial_data) %>%
    stringr::str_remove_all("final_")


  return(financial_data)
}


#' company_id in the ownership tree refer to the company owning the target_company_id
#'
#' @param financial_data financial_data
#' @param ownership_tree ownership_tree
#'
#' @return a dataframe
#'
keep_available_financial_companies_in_ownership_tree <- function(financial_data, ownership_tree) {
  filtered_ownership_tree <- ownership_tree %>%
    dplyr::inner_join(financial_data %>% dplyr::distinct(.data$company_id), by = c("parent_company_id" = "company_id"))
  return(filtered_ownership_tree)
}

#' Lower or equal for min value (0) Stricly higher for the max value (npm==1 is allowed)
#'
#' @param financial_data financial_data
#' @param allowed_range_npm allowed_range_npm
#'
#' @return a dataframe
#'
remove_implausible_values_in_financial_indicators <- function(financial_data, allowed_range_npm) {
  financial_data <- financial_data %>%
    dplyr::mutate(
      net_profit_margin = dplyr::if_else(
        (.data$net_profit_margin <= allowed_range_npm[1]) | (.data$net_profit_margin > allowed_range_npm[2]),
        NA,
        .data$net_profit_margin
      )
    )
  return(financial_data)
}


#' Main financial data preparation script
#'
#' @description Prepare the financial data following those steps :
#'  - Using the country associated to each asset, aggregate the financial indicators from country to region
#'    when a company owns several assets in different countries, but same region.
#'  - Remove implausible values from the raw data, by setting the cell where it appears to NA .
#'    Those values will be filled with averages later on.
#'  - If the ownership tree is provided as an input, match missing financial values of subsidiaries
#'    to their closest parent compahy, if the parent company has non-NA financial values in the raw data.
#'  - Compute averages, and pick the most granular one possible to fill missing values
#'  - As financial indicators are still at the (company_id, ald_sector, region) granularity level,
#'    aggregate those to the company level to be used in the stress test.
#'
#' @param financial_data financial_data
#' @param companies_data companies_data
#' @param ownership_tree ownership_tree
#' @param minimum_sample_size minimum_sample_size
#' @param minimum_ratio_sample minimum_ratio_sample
#' @param allowed_range_npm allowed_range_npm c(min_npm, max_npm)
#'
#' @return a dataframe
#' @export
#'
prepare_financial_data <- function(financial_data, companies_data, ownership_tree, minimum_sample_size, minimum_ratio_sample, allowed_range_npm) {
  #### INITIALISE FINANCIAL DATA
  # add ald_sector provided by asset resolution. This will duplicate rows for companies represented in more than 1 sector.
  financial_data <- add_column_ald_sector_to_financial_data(financial_data, companies_data)

  #### AGGREGATE FINANCIAL DATA FROM ISIN TO COMPANY_ID
  financial_data <- match_location_to_region(financial_data)
  financial_data <- aggregate_financial_indicators(financial_data,
    grp_cols = c("company_id", "ald_sector", "ald_region")
  )

  # Remove implausible values
  financial_data <- remove_implausible_values_in_financial_indicators(financial_data, allowed_range_npm = c(0, 1))


  #### ADD MISSING COMPANIES FROM PRODUCTION
  # add missing companies from production to the financial data and match with closest parent company
  # if it exists in the original financial data
  missing_companies_in_financial_data <- get_missing_companies_in_financial_data(financial_data, companies_data)
  missing_companies_in_financial_data <- match_location_to_region(missing_companies_in_financial_data) %>% dplyr::distinct_all()

  if (!is.null(ownership_tree)) {
    ownership_tree <- keep_available_financial_companies_in_ownership_tree(financial_data, ownership_tree)
    missing_companies_in_financial_data <- match_closest_financial_data_to_missing_companies(
      missing_companies_in_financial_data = missing_companies_in_financial_data,
      financial_data = financial_data,
      ownership_tree = ownership_tree
    )
  }

  #### FILL MISSING VALUES WITH AVERAGES

  # Only use original financial values to compute averages
  financial_averages <- compute_financial_averages(financial_data,
    minimum_sample_size = minimum_sample_size,
    minimum_ratio_sample = minimum_ratio_sample,
    allowed_range_npm = allowed_range_npm
  )

  # Join available and missing financial data before filling empty values with averages
  financial_data_all_companies <- dplyr::bind_rows(
    financial_data,
    missing_companies_in_financial_data
  )
  # # filter rows without a company ID as they're not useful anymore (only has an impact in averages)
  financial_data_all_companies <- financial_data_all_companies %>% dplyr::filter(!is.na(.data$company_id))

  # fill na with averages
  financial_data_all_companies <- add_columns_financial_averages(
    financial_data_all_companies,
    financial_averages[["ald_sector_region_averages"]],
    financial_averages[["ald_sector_averages"]],
    financial_averages[["global_averages"]]
  )

  # fill NA values in financial indicators
  financial_data_all_companies <- select_final_financial_value_using_averages(financial_data_all_companies)
  # aggregate final financial indicators to company level
  prewrangled_financial_data_stress_test <- aggregate_financial_indicators(
    financial_data_all_companies,
    grp_cols = c("company_id")
  )
  financial_data_indicator_types <- aggregate_indicator_types(
    financial_data_all_companies,
    grp_cols = c("company_id")
  )

  prewrangled_financial_data_stress_test <- prewrangled_financial_data_stress_test %>%
    dplyr::inner_join(financial_data_indicator_types, by = "company_id")


  # assert no NA anywhere and no implausible values
  prewrangled_financial_data_stress_test %>%
    remove_implausible_values_in_financial_indicators(allowed_range_npm = c(0, 1)) %>%
    assertr::verify(sum(is.na(.)) == 0)


  return(prewrangled_financial_data_stress_test)
}

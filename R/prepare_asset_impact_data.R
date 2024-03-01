#' read Asset Resolution data
#'
#' @param path_ar_data_raw path to AR excel input
#'
#' @param sheet_name name of excel sheet
#'
read_asset_resolution <- function(path_ar_data_raw, sheet_name) {
  if (sheet_name %in% c("Company Activities", "Company Emissions")) {
    
  
  ar_data <- readxl::read_xlsx(path_ar_data_raw,
                               sheet = sheet_name) %>%
    dplyr::select(-dplyr::starts_with("Direct Ownership")) %>%
    dplyr::rename(
      company_id = .data$`Company ID`,
      company_name = .data$`Company Name`,
      ald_sector = .data$`Asset Sector`,
      technology = .data$`Asset Technology`,
      technology_type = .data$`Asset Technology Type`,
      region = .data$`Asset Region`,
      ald_location = .data$`Asset Country`,
      activity_unit = .data$`Activity Unit`
    )
  } else if (sheet_name == "Company Information") {
    ar_data <- readxl::read_xlsx(path_ar_data_raw,
                                 sheet = sheet_name) %>%
      dplyr::rename(
        company_id = .data$`Company ID`,
        company_name = .data$`Company Name`,
        is_ultimate_parent = .data$`Is Ultimate Parent`,
        ald_location = .data$`Country of Domicile`,
        lei = .data$`LEI`
      )
  } else {
    stop("Sheet name not recognized")
  }
  return(ar_data)
}


#' rename technology column according to some rules
#' @param ar_data ar_data
#'
rename_technology <- function(ar_data) {
  ar_data <- ar_data %>%
    dplyr::mutate(
      technology = dplyr::case_when(
        .data$ald_sector == "Coal" ~ "Coal",
        .data$technology %in% c("Gas", "Natural Gas Liquids") ~ "Gas",
        .data$technology == "Oil and Condensate" ~ "Oil",
        .data$technology == "ICE Diesel" ~ "ICE",
        .data$technology == "ICE Gasoline" ~ "ICE",
        .data$technology == "ICE CNG" ~ "ICE",
        .data$technology == "ICE Propane" ~ "ICE",
        .data$technology == "ICE E85+" ~ "ICE",
        .data$technology == "Hybrid No-Plug" ~ "Hybrid",
        .data$technology == "Hybrid Plug-In" ~ "Hybrid",
        .data$technology == "Fuel Cell" ~ "FuelCell",
        TRUE ~ .data$technology
      )
    )
  return(ar_data)
}


#' Filter out companies with unknown owner
#' @param ar_data ar_data
#'
remove_unknown_owner_companies <- function(ar_data) {
  ar_data <-
    ar_data %>% dplyr::filter(.data$company_name != "Unknown Owner")
  return(ar_data)
}

#' rename ald_sector column according to some rules
#' @param ar_data ar_data
#'
rename_ald_sector <- function(ar_data) {
  ar_data <- ar_data %>%
    dplyr::mutate(ald_sector = dplyr::if_else(.data$ald_sector == "LDV", "Automotive", .data$ald_sector)) %>%
    dplyr::mutate(
      ald_sector = dplyr::case_when(
        .data$technology == "Coal" ~ "Coal",
        .data$technology %in% c("Gas", "Oil") ~ "Oil&Gas",
        TRUE ~ .data$ald_sector
      )
    )
  return(ar_data)
}


#' Sum production and EF values over all columns except technology type.
#' @param ar_data ar_data
#'
aggregate_over_technology_types <- function(ar_data) {
  ar_data <- ar_data %>%
    dplyr::group_by(dplyr::across(-c(
      .data$technology_type
      # removes this column after grouping-dplyr::contains("Equity Ownership ")
    ))) %>%
    dplyr::summarise(dplyr::across(dplyr::contains("Equity Ownership "), .sum_or_all_nans),
                     .groups = "drop")
  return(ar_data)
}


#' Filtering emissions to remove emissions in proportions
#' in order to aggregate the raw values and re-compute the proportions
#'
#' @param company_emissions company_emissions
#'
remove_prop_emissions <- function(company_emissions) {
  company_co2_emissions <- company_emissions %>%
    dplyr::filter(.data$activity_unit %in% c("tCO2e", "tCO2"))

  # Check that all companies have their emissions in raw tCO2 or tCO2e
  # stopifnot(nrow(
  #   company_co2_emissions %>%
  #     dplyr::anti_join(company_emissions, by = dplyr::join_by(
  #       company_id, , company_name, ald_sector, ald_business_unit, ald_location
  #     ))
  # ) == 0)
  return(company_co2_emissions)
}



#' Filter companies by country
#'
#' If filter_hqs and filter_assets are both TRUE,
#' the function will only rowser where both the asset
#' and its HeadQuarters are in the country.
#'
#' @param ar_data tibble, Asset Impact data
#' @param company_informations tibble, company informations
#' @param country_filter character vector, countries to keep in ouput
#' @param filter_hqs logical, whether to only keep the (worldwide)
#'  assets whose HeadQuarters are in the country
#' @param filter_assets logical, wether to only keep the assets in the country
#'
#' @return tibble, raw Asset Impact data
#'
filter_countries_coverage <- function(ar_data,
                                      company_informations,
                                      country_filter = c(),
                                      filter_hqs = FALSE,
                                      filter_assets = FALSE) {
  
  # only filter countries if country_filter is non-empty
  # and at least one of filter_hqs or filter_assets is TRUE
  if ((length(country_filter) > 0) & (filter_hqs | filter_assets)) {
    
    # keep companies with HQs in the selected countries
    # NOTE : SUBSIDIARIES FILTERED OUT
    hqs_in_countries <- company_informations %>%
      dplyr::filter(.data$ald_location %in% country_filter &
                      .data$is_ultimate_parent == TRUE) %>%
      dplyr::distinct(.data$company_id)
    
    
    if (filter_hqs & !filter_assets) {
      # only keep companies with HQs in the selected countries
      # assets belonging to HQs can be worldwide
      ar_data <- ar_data %>%
        dplyr::inner_join(hqs_in_countries, by = c("company_id"))
      
    }
    else if (!filter_hqs & filter_assets) {
      # keep only assets in the country, HQS can be worldwide
      ar_data <- ar_data %>%
        dplyr::filter(.data$ald_location %in% country_filter)
    }
    else if (filter_hqs & filter_assets) {
      # apply both filters
      # only keep companies with HQs in the selected countries and assets in the selected country
      ar_data <- ar_data %>%
        dplyr::inner_join(hqs_in_countries, by = c("company_id")) %>%
        dplyr::filter(.data$ald_location %in% country_filter)
    }
  }
  return(ar_data)
}

#' Prepare Asset Impact data before transformation to abcd
#' @param ar_data_path file path to the source Asset Resolution xlsx
#'
#' @export
prepare_asset_impact_data <- function(ar_data_path) {
  
  # Asset Impact specific data preparation
  
  company_activities <-
    read_asset_resolution(ar_data_path,
                          sheet_name = "Company Activities")
  company_emissions <-
    read_asset_resolution(ar_data_path,
                          sheet_name = "Company Emissions")

  company_activities <- rename_technology(company_activities)
  company_emissions <- rename_technology(company_emissions)

  company_activities <-
    remove_unknown_owner_companies(company_activities)
  company_emissions <-
    remove_unknown_owner_companies(company_emissions)

  company_activities <- rename_ald_sector(company_activities)
  company_emissions <- rename_ald_sector(company_emissions)

  company_activities <-
    aggregate_over_technology_types(company_activities)
  company_emissions <-
    aggregate_over_technology_types(company_emissions)

  company_emissions <- remove_prop_emissions(company_emissions)

  company_activities <- company_activities %>% 
    dplyr::rename(ald_business_unit = .data$technology) %>%
    dplyr::select(-c(.data$region))
  company_emissions <- company_emissions %>% 
    dplyr::rename(ald_business_unit = .data$technology) %>%
    dplyr::select(-c(.data$region))
  
  return(
    list(
      company_activities = company_activities,
      company_emissions = company_emissions
    )
  )

}

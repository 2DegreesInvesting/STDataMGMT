#' read Asset Resolution data
#'
#' @param path_ar_data_raw path to AR excel input
#'
#' @param sheet_name name of excel sheet
#'
read_asset_resolution <- function(path_ar_data_raw, sheet_name) {
  ar_data <- readxl::read_xlsx(path_ar_data_raw,
                               sheet = sheet_name) %>%
    dplyr::select(-dplyr::starts_with("Direct Ownership")) %>%
    dplyr::rename(
      id = .data$`Company ID`,
      company_name = .data$`Company Name`,
      ald_sector = .data$`Asset Sector`,
      technology = .data$`Asset Technology`,
      technology_type = .data$`Asset Technology Type`,
      region = .data$`Asset Region`,
      ald_location = .data$`Asset Country`,
      activity_unit = .data$`Activity Unit`
    )
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

#' Prepare Asset Resolution data before transformation to abcd
#' @param ar_data_path file path to the source Asset Resolution xlsx
#'
#' @export
prepare_asset_impact_data <- function(ar_data_path) {
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

  company_activities <- company_activities %>% dplyr::rename(ald_business_unit = .data$technology,
                                                             company_id = .data$id) %>%
                                                             dplyr::select(-c(.data$region))
  company_emissions <- company_emissions %>% dplyr::rename(ald_business_unit = .data$technology,
                                                           company_id = .data$id)%>%
                                                             dplyr::select(-c(.data$region))

  return(
    list(
      company_activities = company_activities,
      company_emissions = company_emissions
    )
  )

}

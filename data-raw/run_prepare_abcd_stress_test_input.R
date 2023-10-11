# This script generates the abcd input from asset_resolution data
# as well as the production_type.rda reference dataset in this packge
# TODO all the code before abcd_data should be translated to SQL

devtools::load_all()

## PARAMETERS
path_ar_data_raw <-
  r2dii.utils::path_dropbox_2dii(
    "ST_INPUTS",
    "ST_INPUTS_PRODUCTION"
  )

output_path_stress_test_input <-
  r2dii.utils::path_dropbox_2dii(
    "ST_INPUTS",
    "ST_INPUTS_MASTER"
  )

start_year <- 2022
time_horizon <- 5
additional_year <- NULL
sector_list <- c("Automotive", "Power", "Oil&Gas", "Coal")
km_per_vehicle <- 15000

#' read Asset Resolution data
#'
#' @param path_ar_data_raw path to AR excel input
#'
#' @param sheet_name name of excel sheet
#'
read_asset_resolution <- function(path_ar_data_raw, sheet_name) {
  ar_data <- readxl::read_xlsx(path_ar_data_raw,
    sheet = sheet_name
  ) %>%
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


company_activities <-
  read_asset_resolution(
    fs::path(path_ar_data_raw,
      "AR-Company-Indicators_2022Q4",
      ext = "xlsx"
    ),
    sheet_name = "Company Activities"
  )
company_emissions <-
  read_asset_resolution(
    fs::path(path_ar_data_raw,
      "AR-Company-Indicators_2022Q4",
      ext = "xlsx"
    ),
    sheet_name = "Company Emissions"
  )

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


## RENAME
company_activities <- rename_technology(company_activities)
company_emissions <- rename_technology(company_emissions)

#' Filter out companies with unknown owner
#' @param ar_data ar_data
#'
remove_unknown_owner_companies <- function(ar_data) {
  ar_data <-
    ar_data %>% dplyr::filter(.data$company_name != "Unknown Owner")
  return(ar_data)
}
company_activities <- remove_unknown_owner_companies(company_activities)
company_emissions <- remove_unknown_owner_companies(company_emissions)

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

company_activities <- rename_ald_sector(company_activities)
company_emissions <- rename_ald_sector(company_emissions)

#' Sum production and EF values over all columns except technology type.
#' @param abcd_data abcd_data
#'
aggregate_over_technology_types <- function(ar_data) {
  ar_data <- ar_data %>%
    dplyr::group_by(dplyr::across(
      c(
        -.data$technology_type, # removes this column after grouping
        -dplyr::contains("Equity Ownership ")
      )
    )) %>%
    dplyr::summarise(
      across(dplyr::contains("Equity Ownership "), .sum_or_all_nans),
      .groups = "drop"
    )
  return(ar_data)
}

company_activities <- aggregate_over_technology_types(company_activities)
company_emissions <- aggregate_over_technology_types(company_emissions)


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
company_emissions <- remove_prop_emissions(company_emissions)


company_activities <- company_activities %>% dplyr::rename(
  ald_business_unit = technology,
  company_id = id
)
company_emissions <- company_emissions %>% dplyr::rename(
  ald_business_unit = technology,
  company_id = id
)

# THE PART ABOVE IS ASSET RESOLUTION SPECIFIC
# ========================
# CONVERT CLEAN COMPANY DATA TO ABCD BELOW

abcd_stress_test_input <-
  prepare_abcd_data(
    company_activities = company_activities,
    company_emissions = company_emissions,
    scenarios_geographies = scenarios_geographies,
    start_year = start_year,
    time_horizon = time_horizon,
    additional_year = additional_year,
    km_per_vehicle = km_per_vehicle,
    sector_list = sector_list
  )

abcd_stress_test_input %>%
  assertr::verify(all(colSums(is.na(.)) == 0))

# usethis::use_data(abcd_stress_test_input, overwrite=T)

abcd_stress_test_input %>% readr::write_csv(fs::path(
  output_path_stress_test_input,
  "abcd_stress_test_input",
  ext = "csv"
))

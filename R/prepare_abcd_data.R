#' Sum values by skipping nans, or return nan if all values are nan
#' (instead of 0 with sum(... na.rm=T))
#'
#' @param array a vector of numbers
#'
#' @return sum of array or nan
#' @export
#'
.sum_or_all_nans <- function(array) {
  if (all(is.na(array))) {
    return(NA)
  } else {
    return(sum(array, na.rm = TRUE))
  }
}

#' read Asset Resolution data
#'
#' @param path_ar_data_raw path to AR excel input
#'
#' @param sheet_name name of excel sheet
#'
#' @export
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

#' pivot values of Equity Ownership, to be used as yearly production/emissions
#' TODO when Direct Ownership is not nan, use it when Equity Ownership is nan ?
#'
#' @param ar_data ar_data
#'
#' @export
pivot_equity_ownership_columns <- function(ar_data) {
  ar_data <- ar_data %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("Equity Ownership "),
      names_to = "year",
      values_to = "equity_ownership"
    ) %>%
    dplyr::mutate(year = stringr::str_extract(.data$year, stringr::regex("\\d+")))

  return(ar_data)
}

#' Filter out companies with unknown owner
#' @param ar_data ar_data
#'
#' @export
remove_unknown_owner_companies <- function(ar_data) {
  ar_data <-
    ar_data %>% dplyr::filter(.data$company_name != "Unknown Owner")
  return(ar_data)
}

#' Filtering emissions to remove emissions in proportions
#' in order to aggregate the raw values and re-compute the proportions
#'
#' @param company_emissions company_emissions
#'
#' @export
remove_prop_emissions <- function(company_emissions) {
  company_co2_emissions <- company_emissions %>%
    dplyr::filter(.data$activity_unit %in% c("tCO2e", "tCO2"))

  # Check that all companies have their emissions in raw tCO2 or tCO2e
  stopifnot(nrow(
    company_co2_emissions %>%
      dplyr::distinct(
        .data$id,
        .data$company_name,
        .data$ald_sector,
        .data$technology,
        .data$technology_type,
        .data$technology_type,
        .data$region,
        .data$ald_location
      ) %>%
      dplyr::anti_join(company_emissions)
  ) == 0)
  return(company_co2_emissions)
}

#' rename technology column according to some rules
#' @param ar_data ar_data
#'
#' @export
rename_technology <- function(ar_data) {
  ar_data <- ar_data %>%
    dplyr::mutate(
      technology = dplyr::case_when(
        .data$ald_sector == "Coal" ~ "Coal",
        .data$technology %in% c("Gas", "Natural Gas Liquids") ~ "Gas",
        .data$technology == "Oil and Condensate" ~ "Oil",
        TRUE ~ .data$technology
      )
    )
  return(ar_data)
}

#' rename ald_sector column according to some rules
#' @param ar_data ar_data
#'
#' @export
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

#' aggregate volumes of equity_ownership over duplicated rows
#'
#' @param ar_data ar_data
#'
#' @export
aggregate_equity_ownership_after_renaming <- function(ar_data) {
  ar_data <- ar_data %>%
    dplyr::group_by(
      .data$id,
      .data$company_name,
      .data$ald_sector,
      .data$technology,
      .data$technology_type,
      .data$region,
      .data$ald_location,
      .data$activity_unit,
      .data$year
    ) %>%
    dplyr::summarise(equity_ownership = .sum_or_all_nans(.data$equity_ownership)) %>%
    dplyr::ungroup()

  return(ar_data)
}

#' Merge production and emissions data.
#' Filter rows where the production unit and emission unit match as expected
#' @param company_activities company_activities dataframe
#' @param company_emissions company_emissions dataframe
#'
#' @export
match_emissions_to_production <- function(company_activities,
                                          company_emissions) {
  company_activities <- company_activities %>%
    dplyr::rename(
      ald_production_unit = .data$activity_unit,
      ald_production = .data$equity_ownership
    )
  company_emissions <- company_emissions %>%
    dplyr::rename(
      emissions_factor_unit = .data$activity_unit,
      emissions_factor = .data$equity_ownership
    )

  abcd_data <-
    dplyr::left_join(company_activities, company_emissions)

  return(abcd_data)
}

#' #' Sum production values for technologies that have the same name, after
#' #' for example a renaming has been applied
#' #' @param abcd_data
#' #'
#' #' @export
#' aggregate_production_after_renaming <- function(abcd_data) {
#'   abcd_data <- abcd_data %>%
#'     dplyr::group_by(
#'       .data$id,
#'       .data$company_name,
#'       .data$ald_sector,
#'       .data$technology,
#'       .data$technology_type,
#'       .data$region,
#'       .data$ald_location,
#'       .data$ald_production_unit,
#'       .data$year
#'     ) %>%
#'     dplyr::summarise(ald_production = sum(.data$ald_production, na.rm = T))
#'   return(abcd_data)
#' }

#' filter to keep only desired years
#'
#' @param abcd_data abcd_data
#' @param start_year start_year
#' @param time_horizon time_horizon
#' @param additional_year additional_year
#'
#' @export
filter_years_abcd_data <- function(abcd_data,
                                   start_year,
                                   time_horizon,
                                   additional_year) {
  abcd_data <- abcd_data %>%
    dplyr::filter(.data$year %in% c(start_year:(start_year + time_horizon), additional_year))
  return(abcd_data)
}

#' use avg EFs per technology to fill missing values
#'
#' @param abcd_data abcd_data
#'
#' @export
fill_missing_emission_factor <- function(abcd_data) {
  avg_emission_factors <- abcd_data %>%
    dplyr::group_by(.data$ald_sector,
                    .data$technology,
                    .data$emissions_factor_unit) %>%
    dplyr::summarise(emissions_factor = mean(.data$emissions_factor, na.rm = T)) %>%
    dplyr::ungroup()

  abcd_missing_ef <- abcd_data %>%
    dplyr::filter(is.na(.data$emissions_factor))

  abcd_missing_ef <- abcd_missing_ef %>%
    dplyr::select(-.data$emissions_factor, -.data$emissions_factor_unit) %>%
    dplyr::left_join(avg_emission_factors,
                     by = c("ald_sector", "technology"))

  # Fill nans when there is no avg emission factor for some technologies
  # TODO why is there no emission factor on HDV ?
  # abcd_missing_ef <-
  #   abcd_missing_ef %>%
  #   dplyr::mutate(emissions_factor = tidyr::replace_na(emissions_factor, 0))

  abcd_data <- abcd_data %>%
    dplyr::filter(!is.na(.data$emissions_factor)) %>%
    dplyr::bind_rows(abcd_missing_ef)

  return(abcd_data)
}

#' convert EF from tCO2 (or tCO2e) to the ratio of tCO2 (or tCO2e) over production
#'
#' @param abcd_data abcd_data
#'
#' @export
recreate_prop_emissions <- function(abcd_data) {
  # note : It appears that AR data assumes that vehicles will
  # drive 15000 km to compute the CO2/km emission factor
  # TODO check if this is true on every vehicle technology
  abcd_data <- abcd_data %>% dplyr::mutate(
    ald_production = dplyr::if_else(
      .data$ald_production_unit == "# vehicles",
      .data$ald_production * 15000,
      .data$ald_production
    ),
    ald_production_unit = dplyr::if_else(
      .data$ald_production_unit == "# vehicles",
      "km",
      .data$ald_production_unit
    )
  )

  abcd_data <-
    abcd_data %>% dplyr::mutate(
      emissions_factor = .data$emissions_factor / .data$ald_production,
      emissions_factor_unit = dplyr::if_else(
        !is.na(.data$emissions_factor_unit),
        paste(
          .data$emissions_factor_unit,
          .data$ald_production_unit,
          sep = "/"
        ),
        NA
      )
    )

  # match the MW production to tCO2/MWh emissions
  abcd_MW_prod <- abcd_data %>%
    dplyr::filter(.data$ald_production_unit == "MW") %>%
    dplyr::select(-.data$emissions_factor_unit, -.data$emissions_factor)
  abcd_MWh_emissions <- abcd_data %>%
    dplyr::filter(.data$ald_production_unit == "MWh") %>%
    dplyr::select(-.data$ald_production_unit, -.data$ald_production)
  abcd_MW_prod_MWh_emissions <-
    dplyr::inner_join(abcd_MW_prod, abcd_MWh_emissions)
  abcd_data <- dplyr::bind_rows(
    abcd_data %>%
      dplyr::filter(!.data$ald_production_unit %in% c("MW", "MWh")),
    abcd_MW_prod_MWh_emissions
  )

  return(abcd_data)
}


#' Sum production and EF values over all columns except technology type.
#' @param abcd_data abcd_data
#'
#' @export
aggregate_technology_types <- function(abcd_data) {
  abcd_data <- abcd_data %>%
    dplyr::group_by(
      .data$id,
      .data$company_name,
      .data$region,
      .data$ald_location,
      .data$ald_sector,
      .data$technology,
      .data$year,
      .data$ald_production_unit,
      .data$emissions_factor_unit
    ) %>%
    dplyr::summarise(
      emissions_factor = .sum_or_all_nans(.data$emissions_factor),
      ald_production = .sum_or_all_nans(.data$ald_production)
    )
  return(abcd_data)
}

#' Drop rows where production or emission are nan
#' @param abcd_data abcd_data
#'
#' @export
drop_empty_prod_and_ef <- function(abcd_data) {
  abcd_data <- abcd_data %>%
    dplyr::filter(!is.na(.data$ald_production) &
                    !is.na(.data$emissions_factor))
  return(abcd_data)
}

#' Duplicate rows according to the available geographies
#' @param abcd_data abcd_data
#'
#' @param bench_regions bench_regions
#' @param .default .default
#' @param .iso2c .iso2c
#'
#' @export
expand_by_scenario_geography <-
  function(abcd_data,
           bench_regions,
           .default = "Global",
           .iso2c = "ald_location") {
    stopifnot(.iso2c %in% names(abcd_data))

    # TODO skip regions in the dataloading, never used in preprocessing
    abcd_data <- abcd_data %>%
      dplyr::select(-.data$region)

    dict <-
      bench_regions %>%
      dplyr::select(.data$country_iso, .data$scenario_geography) %>%
      dplyr::distinct()

    abcd_data <- abcd_data %>%
      dplyr::left_join(dict, by = stats::setNames("country_iso", .iso2c)) %>%
      dplyr::mutate(
        scenario_geography = dplyr::case_when(
          is.na(.data$scenario_geography) ~ .default,
          .data$scenario_geography == "" ~ .default,
          TRUE ~ .data$scenario_geography
        )
      )
    return(abcd_data)
  }

#' Fill ald_production and emissions_factor with values of previous years
#' for a given technology at a company
#'
#' @param abcd_data abcd_data
#'
#' @export
fill_empty_years_that_follows <- function(abcd_data) {
  abcd_data <- abcd_data %>%
    dplyr::arrange(
      .data$id,
      .data$ald_location,
      .data$ald_sector,
      .data$technology,
      .data$ald_production_unit,
      .data$emissions_factor_unit,
      .data$year # TODO arrange only on years => same result
    ) %>%
    dplyr::group_by(
      .data$id,
      .data$ald_location,
      .data$ald_sector,
      .data$technology,
      .data$emissions_factor_unit,
      .data$ald_production_unit
    ) %>%
    tidyr::fill(.data$ald_production, .direction = "downup") %>%
    tidyr::fill(.data$emissions_factor, .direction = "downup") %>%
    dplyr::ungroup()
  tidyr::fill
  return(abcd_data)
}

#' rename columns, and sum ald_production over each company to create
#' plan_sec_prod column
#' @param abcd_data abcd_data
#'
#' @export
create_plan_prod_columns <- function(abcd_data) {
  abcd_data <- abcd_data %>%
    dplyr::rename(
      plan_tech_prod = .data$ald_production,
      plan_emission_factor = .data$emissions_factor
    )

  abcd_data <- abcd_data %>%
    dplyr::group_by(.data$id,
                    .data$company_name,
                    .data$scenario_geography,
                    .data$year,
                    .data$ald_sector) %>%
    dplyr::mutate(plan_sec_prod = sum(.data$plan_tech_prod, na.rm = TRUE)) %>%
    dplyr::ungroup()

  abcd_data <- abcd_data %>% dplyr::select(
    .data$id,
    .data$company_name,
    .data$scenario_geography,
    .data$year,
    .data$ald_sector,
    .data$technology,
    .data$plan_tech_prod,
    .data$plan_emission_factor,
    .data$plan_sec_prod
  )

  return(abcd_data)
}

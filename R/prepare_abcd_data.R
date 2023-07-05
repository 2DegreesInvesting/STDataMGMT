.sum_or_all_nans <- function(array) {
  if (all(is.na(array))) {
    return(NA)
  } else {
    return(sum(array, na.rm = TRUE))
  }
}

read_asset_resolution <- function(path_ar_data_raw, sheet_name) {
  ar_data <- readxl::read_xlsx(path_ar_data_raw,
                               sheet = sheet_name
  ) %>%
    dplyr::select(-dplyr::starts_with("Direct Ownership")) %>%
    dplyr::rename(
      id = `Company ID`,
      company_name = `Company Name`,
      ald_sector = `Asset Sector`,
      technology = `Asset Technology`,
      technology_type = `Asset Technology Type`,
      region = `Asset Region`,
      ald_location = `Asset Country`,
      activity_unit = `Activity Unit`
    )
  return(ar_data)
}

#' pivot values of Equity Ownership, to be used as yearly production/emissions
#' TODO when Direct Ownership is not nan, use it when Equity Ownership is nan ?
pivot_equity_ownership_columns <- function(ar_data) {
  ar_data <- ar_data %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("Equity Ownership "),
      names_to = "year",
      values_to = "equity_ownership"
    ) %>%
    dplyr::mutate(year = stringr::str_extract(year, stringr::regex("\\d+")))

  return(ar_data)
}

remove_unknown_owner_companies <- function(ar_data) {
  ar_data <-
    ar_data %>% dplyr::filter(company_name != "Unknown Owner")
  return(ar_data)
}

#' Filtering emissions to remove emissions in proportions
#' in order to aggregate the raw values and re-compute the proportions
remove_prop_emissions <- function(company_emissions) {
  company_co2_emissions <- company_emissions %>%
    dplyr::filter(activity_unit %in% c("tCO2e", "tCO2"))

  # Check that all companies have their emissions in raw tCO2 or tCO2e
  stopifnot(nrow(
    company_co2_emissions %>%
      dplyr::distinct(
        id,
        company_name,
        ald_sector,
        technology,
        technology_type,
        technology_type,
        region,
        ald_location
      ) %>%
      dplyr::anti_join(company_emissions)
  ) == 0)
  return(company_co2_emissions)
}

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

rename_ald_sector <- function(ar_data) {
  ar_data <- ar_data %>%
    dplyr::mutate(ald_sector = dplyr::if_else(.data$ald_sector == "LDV", "Automotive", .data$ald_sector)) %>%
    dplyr::mutate(
      ald_sector = dplyr::case_when(
        technology == "Coal" ~ "Coal",
        technology %in% c("Gas", "Oil") ~ "Oil&Gas",
        TRUE ~ ald_sector
      )
    )
  return(ar_data)
}

#' aggregate volumes of equity_ownership over duplicated rows
aggregate_equity_ownership_after_renaming <- function(ar_data) {
  ar_data <- ar_data %>%
    dplyr::group_by(
      id,
      company_name,
      ald_sector,
      technology,
      technology_type,
      region,
      ald_location,
      activity_unit,
      year
    ) %>%
    dplyr::summarise(equity_ownership = .sum_or_all_nans(equity_ownership)) %>%
    dplyr::ungroup()

  return(ar_data)
}

#' Merge production and emissions data.
#' Filter rows where the production unit and emission unit match as expected
match_emissions_to_production <- function(company_activities,
                                          company_emissions) {
  company_activities <- company_activities %>%
    dplyr::rename(
      ald_production_unit = activity_unit,
      ald_production = equity_ownership
    )
  company_emissions <- company_emissions %>%
    dplyr::rename(
      emissions_factor_unit = activity_unit,
      emissions_factor = equity_ownership
    )

  abcd_data <-
    dplyr::left_join(company_activities, company_emissions)

  return(abcd_data)
}

aggregate_production_after_renaming <- function(abcd_data) {
  abcd_data <- abcd_data %>%
    dplyr::group_by(
      id,
      company_name,
      ald_sector,
      technology,
      technology_type,
      region,
      ald_location,
      ald_production_unit,
      year
    ) %>%
    dplyr::summarise(ald_production = sum(ald_production, na.rm = T))
  return(abcd_data)
}

filter_years_abcd_data <- function(abcd_data,
                                   start_year,
                                   time_horizon,
                                   additional_year) {
  abcd_data <- abcd_data %>%
    dplyr::filter(year %in% c(start_year:(start_year + time_horizon), additional_year))
  return(abcd_data)
}

#' use avg EFs per technology to fill missing values
fill_missing_emission_factor <- function(abcd_data) {
  avg_emission_factors <- abcd_data %>%
    dplyr::group_by(
      ald_sector,
      technology,
      emissions_factor_unit
    ) %>%
    dplyr::summarise(emissions_factor = mean(emissions_factor, na.rm = T)) %>%
    dplyr::ungroup()

  abcd_missing_ef <- abcd_data %>%
    dplyr::filter(is.na(emissions_factor))

  abcd_missing_ef <- abcd_missing_ef %>%
    dplyr::select(-emissions_factor, -emissions_factor_unit) %>%
    dplyr::left_join(avg_emission_factors,
                     by = c("ald_sector", "technology")
    )

  # Fill nans when there is no avg emission factor for some technologies
  # TODO why is there no emission factor on HDV ?
  # abcd_missing_ef <-
  #   abcd_missing_ef %>%
  #   dplyr::mutate(emissions_factor = tidyr::replace_na(emissions_factor, 0))

  abcd_data <- abcd_data %>%
    dplyr::filter(!is.na(emissions_factor)) %>%
    dplyr::bind_rows(abcd_missing_ef)

  return(abcd_data)
}

#' convert EF from tCO2 (or tCO2e) to the ratio of tCO2 (or tCO2e) over production
#'
recreate_prop_emissions <- function(abcd_data) {
  # note : It appears that AR data assumes that vehicles will
  # drive 15000 km to compute the CO2/km emission factor
  # TODO check if this is true on every vehicle technology
  abcd_data <- abcd_data %>% dplyr::mutate(
    ald_production = dplyr::if_else(
      ald_production_unit == "# vehicles",
      ald_production * 15000,
      ald_production
    ),
    ald_production_unit = dplyr::if_else(
      ald_production_unit == "# vehicles",
      "km",
      ald_production_unit
    )
  )

  abcd_data <-
    abcd_data %>% dplyr::mutate(
      emissions_factor = emissions_factor / ald_production,
      emissions_factor_unit = dplyr::if_else(
        !is.na(emissions_factor_unit),
        paste(emissions_factor_unit,
              ald_production_unit,
              sep = "/"
        ),
        NA
      )
    )

  # match the MW production to tCO2/MWh emissions
  abcd_MW_prod <- abcd_data %>%
    dplyr::filter(ald_production_unit == "MW") %>%
    dplyr::select(-emissions_factor_unit, -emissions_factor)
  abcd_MWh_emissions <- abcd_data %>%
    dplyr::filter(ald_production_unit == "MWh") %>%
    dplyr::select(-ald_production_unit, -ald_production)
  abcd_MW_prod_MWh_emissions <-
    dplyr::inner_join(abcd_MW_prod, abcd_MWh_emissions)
  abcd_data <- dplyr::bind_rows(
    abcd_data %>%
      dplyr::filter(!ald_production_unit %in% c("MW", "MWh")),
    abcd_MW_prod_MWh_emissions
  )

  return(abcd_data)
}

aggregate_technology_types <- function(abcd_data) {
  abcd_data <- abcd_data %>%
    dplyr::group_by(
      id,
      company_name,
      region,
      ald_location,
      ald_sector,
      technology,
      year,
      ald_production_unit,
      emissions_factor_unit
    ) %>%
    dplyr::summarise(
      emissions_factor = .sum_or_all_nans(emissions_factor),
      ald_production = .sum_or_all_nans(ald_production)
    )
  return(abcd_data)
}

drop_empty_prod_and_ef <- function(abcd_data) {
  abcd_data <- abcd_data %>%
    dplyr::filter(!is.na(ald_production) & !is.na(emissions_factor))
  return(abcd_data)
}

expand_by_scenario_geography <-
  function(abcd_data,
           bench_regions,
           .default = "Global",
           .iso2c = "ald_location") {
    stopifnot(.iso2c %in% names(abcd_data))

    # TODO skip regions in the dataloading, never used in preprocessing
    abcd_data <- abcd_data %>%
      dplyr::select(-region)

    dict <-
      bench_regions %>%
      dplyr::select(country_iso, scenario_geography) %>%
      dplyr::distinct()

    abcd_data %>%
      dplyr::left_join(dict, by = setNames("country_iso", .iso2c)) %>%
      dplyr::mutate(
        scenario_geography = dplyr::case_when(
          is.na(scenario_geography) ~ .default,
          scenario_geography == "" ~ .default,
          TRUE ~ scenario_geography
        )
      )
  }

#' Fill ald_production and emissions_factor with values of previous years
#' for a given technology at a company
fill_empty_years_that_follows <- function(abcd_data) {
  abcd_data <- abcd_data %>%
    dplyr::arrange(
      id,
      ald_location,
      ald_sector,
      technology,
      ald_production_unit,
      emissions_factor_unit,
      year
    ) %>%
    dplyr::group_by(
      id,
      ald_location,
      ald_sector,
      technology,
      emissions_factor_unit,
      ald_production_unit
    ) %>%
    tidyr::fill(ald_production, .direction = "downup") %>%
    tidyr::fill(emissions_factor, .direction = "downup") %>%
    dplyr::ungroup()

  return(abcd_data)
}

create_plan_prod_columns <- function(abcd_data) {
  abcd_data <- abcd_data %>%
    dplyr::rename(plan_tech_prod = ald_production)

  abcd_data <- abcd_data %>%
    dplyr::group_by(id, company_name, scenario_geography, year, ald_sector) %>%
    dplyr::mutate(plan_sec_prod = sum(plan_tech_prod, na.rm = TRUE)) %>%
    dplyr::ungroup()

  return(abcd_data)
}

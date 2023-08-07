#' Sum values by skipping nans, or return nan if all values are nan
#' (instead of 0 with sum(... na.rm=T))
#'
#' @param array a vector of numbers
#'
#' @return sum of array or nan
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

#' pivot values of Equity Ownership, to be used as yearly production/emissions
#' TODO when Direct Ownership is not nan, use it when Equity Ownership is nan ?
#'
#' @param ar_data ar_data
#'
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
aggregate_equity_ownership_after_renaming <- function(ar_data) {
  ar_data <- ar_data %>%
    dplyr::group_by(dplyr::across(c(-.data$equity_ownership))) %>%
    dplyr::summarise(equity_ownership = .sum_or_all_nans(.data$equity_ownership)) %>%
    dplyr::ungroup()

  return(ar_data)
}

#' Merge production and emissions data.
#' Filter rows where the production unit and emission unit match as expected
#' @param company_activities company_activities dataframe
#' @param company_emissions company_emissions dataframe
#'
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
    dplyr::full_join(company_activities, company_emissions)

  return(abcd_data)
}

#' filter to keep only desired years
#'
#' @param abcd_data abcd_data
#' @param start_year start_year
#' @param time_horizon time_horizon
#' @param additional_year additional_year
#'
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
fill_missing_emission_factor <- function(abcd_data) {
  avg_emission_factors <- abcd_data %>%
    dplyr::group_by(
      .data$ald_sector,
      .data$technology,
      .data$emissions_factor_unit
    ) %>%
    dplyr::summarise(emissions_factor = mean(.data$emissions_factor, na.rm = T)) %>%
    dplyr::ungroup()

  abcd_missing_ef <- abcd_data %>%
    dplyr::filter(is.na(.data$emissions_factor))

  abcd_missing_ef <- abcd_missing_ef %>%
    dplyr::select(-.data$emissions_factor, -.data$emissions_factor_unit) %>%
    dplyr::left_join(avg_emission_factors,
      by = c("ald_sector", "technology")
    )

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
#' @param km_per_vehicle  It appears that AR data assumes that vehicles will
#'    drive 15000 km to compute the CO2/km emission factor
#'    TODO check if this is true on every vehicle technology
#'
create_emissions_factor_ratio <- function(abcd_data, km_per_vehicle) {
  # note : It appears that AR data assumes that vehicles will
  # drive 15000 km to compute the CO2/km emission factor
  # TODO check if this is true on every vehicle technology
  abcd_data <- abcd_data %>% dplyr::mutate(
    ald_production = dplyr::if_else(
      .data$ald_production_unit == "# vehicles",
      .data$ald_production * km_per_vehicle,
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
aggregate_over_technology_types <- function(abcd_data) {
  abcd_data <- abcd_data %>%
    dplyr::group_by(dplyr::across(
      c(
        -.data$technology_type, -.data$ald_production, -.data$emissions_factor
      )
    )) %>%
    dplyr::summarise(
      emissions_factor = .sum_or_all_nans(.data$emissions_factor),
      ald_production = .sum_or_all_nans(.data$ald_production)
    ) %>%
    dplyr::ungroup()
  return(abcd_data)
}

#' Drop rows where production OR emission are nan
#' @param abcd_data abcd_data
#'
drop_empty_prod_or_ef <- function(abcd_data) {
  nan_on_all_years <- abcd_data %>%
    dplyr::group_by(dplyr::across(
      c(
        -.data$year,
        -.data$ald_production,
        -.data$emissions_factor
      )
    )) %>%
    dplyr::summarise(
      all_nans_prod = all(is.na(.data$ald_production)),
      all_nans_emiss = all(is.na(.data$emissions_factor))
    ) %>%
    dplyr::ungroup()

  rows_to_drop <- nan_on_all_years %>%
    dplyr::filter(.data$all_nans_prod | .data$all_nans_emiss) %>%
    dplyr::select(c(-.data$all_nans_prod, -.data$all_nans_emiss))

  abcd_data <- abcd_data %>% dplyr::anti_join(rows_to_drop)

  return(abcd_data)
}

#' Duplicate rows according to the available geographies
#' @param abcd_data abcd_data
#'
#' @param bench_regions bench_regions
#' @param .default .default
#' @param .iso2c .iso2c
#'
expand_by_scenario_geography <-
  function(abcd_data,
           bench_regions,
           .default = "Global",
           .iso2c = "ald_location") {
    stopifnot(.iso2c %in% names(abcd_data))

    abcd_data <- abcd_data %>%
      dplyr::select(-.data$region)

    dict <-
      bench_regions %>%
      dplyr::select(.data$country_iso, .data$scenario_geography_newname) %>%
      dplyr::rename(scenario_geography = .data$scenario_geography_newname) %>%
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

#' Sum productions and emissions over all other columns.
#' This is done to aggregate those metrics over the different
#' countries in a region where a company operates in.
#'
#' @param abcd_data abcd_data
#'
#' @return abcd_data
aggregate_over_locations <- function(abcd_data) {
  abcd_data <- abcd_data %>%
    dplyr::group_by(dplyr::across(
      c(
        -.data$ald_location, -.data$ald_production, -.data$emissions_factor
      )
    )) %>%
    dplyr::summarise(
      ald_production = .sum_or_all_nans(.data$ald_production),
      emissions_factor = .sum_or_all_nans(.data$emissions_factor)
    ) %>%
    dplyr::ungroup()
  return(abcd_data)
}

#' Fill ald_production and emissions_factor with values of previous years
#' for a given technology at a company
#'
#' @param abcd_data abcd_data
#'
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

  return(abcd_data)
}

#' rename columns, and sum ald_production over each company to create
#' plan_sec_prod column
#' @param abcd_data abcd_data
#'
create_plan_prod_columns <- function(abcd_data) {
  abcd_data <- abcd_data %>%
    dplyr::rename(
      plan_tech_prod = .data$ald_production,
      plan_emission_factor = .data$emissions_factor
    )

  abcd_data <- abcd_data %>%
    dplyr::group_by(
      .data$id,
      .data$company_name,
      .data$scenario_geography,
      .data$year,
      .data$ald_sector
    ) %>%
    dplyr::mutate(plan_sec_prod = sum(.data$plan_tech_prod, na.rm = TRUE)) %>%
    dplyr::ungroup()

  return(abcd_data)
}

#' Filter dataframe to keep only desired sectors
#' @param abcd_data abcd_data
#' @param sector_list list of sectors to keep
#'
filter_sectors_abcd_data <- function(abcd_data, sector_list) {
  abcd_data <- abcd_data %>%
    dplyr::filter(.data$ald_sector %in% sector_list)
  return(abcd_data)
}



#' Title
#'
#' @param company_activities company_activities
#' @param company_emissions company_emissions
#'
#' @return company_activities, company_emissions
#' @export
prepare_assets_data <-
  function(company_activities, company_emissions) {
    #### PREPROCESS ASSET RESOLUTION DATA
    ## TRANSFORM
    company_activities <-
      pivot_equity_ownership_columns(company_activities)
    company_emissions <-
      pivot_equity_ownership_columns(company_emissions)

    ## FILTERING
    company_activities <-
      remove_unknown_owner_companies(company_activities)
    company_emissions <-
      remove_unknown_owner_companies(company_emissions)

    company_emissions <- remove_prop_emissions(company_emissions)

    ## RENAME
    company_activities <- rename_technology(company_activities)
    company_emissions <- rename_technology(company_emissions)

    company_activities <- rename_ald_sector(company_activities)
    company_emissions <- rename_ald_sector(company_emissions)

    ## AGGREGATIONS
    company_activities <-
      aggregate_equity_ownership_after_renaming(company_activities)
    company_emissions <-
      aggregate_equity_ownership_after_renaming(company_emissions)

    return(
      list(
        company_activities = company_activities,
        company_emissions = company_emissions
      )
    )
  }


#' Title
#'
#' @param company_activities company_activities
#' @param company_emissions company_emissions
#' @param scenarios_geographies scenarios_geographies
#' @param start_year start_year
#' @param time_horizon time_horizon
#' @param additional_year additional_year
#' @param sector_list sector_list
#'
#' @return abcd_data
#' @export
prepare_abcd_data <- function(company_activities,
                              company_emissions,
                              scenarios_geographies,
                              start_year,
                              time_horizon,
                              additional_year,
                              sector_list) {
  ###### ABCD

  ## DATALOAD
  abcd_data <-
    match_emissions_to_production(company_activities, company_emissions)

  rm(company_activities, company_emissions)

  ## AGGREGATIONS

  abcd_data <- aggregate_over_technology_types(abcd_data)

  abcd_data <- fill_empty_years_that_follows(abcd_data)

  # at this point, nans in ald_production are only due to fully empty production in raw data
  # to check that, only 2 values with this command:
  #   abcd_data %>% group_by(id, company_name, region, ald_location, ald_sector, technology, ald_production_unit, emissions_factor_unit) %>% summarise(nna=sum(is.na(ald_production))) %>% ungroup() %>% distinct(nna)

  abcd_data <-
    expand_by_scenario_geography(abcd_data, scenarios_geographies)
  abcd_data <- aggregate_over_locations(abcd_data)

  abcd_data <- create_emissions_factor_ratio(abcd_data)
  abcd_data <- fill_missing_emission_factor(abcd_data)

  # nans in emission_factor only on all years of a given thech (same as above)
  # to check :
  #  abcd_data %>% group_by(id, company_name, region, ald_location, ald_sector, technology, ald_production_unit, emissions_factor_unit) %>% summarise(nna=sum(is.na(emissions_factor))) %>% ungroup() %>% distinct(nna)

  abcd_data <- drop_empty_prod_or_ef(abcd_data)

  abcd_data <- create_plan_prod_columns(abcd_data)

  ## FILTERINGS
  abcd_data <-
    filter_sectors_abcd_data(abcd_data, sector_list = sector_list)
  abcd_data <-
    filter_years_abcd_data(
      abcd_data,
      start_year = start_year,
      time_horizon = time_horizon,
      additional_year = additional_year
    )


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

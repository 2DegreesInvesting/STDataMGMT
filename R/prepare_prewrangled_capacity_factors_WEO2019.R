#' This function reads scenario data in the form of data as found in the dropbox
#' under "Raw Data", extracts the relevant information for power capacity factors
#' and prepares these in the format required in the stress test. Currently
#' expects to get data for up to year 2040.
#'
#' @param data Tibble that provides raw scenario data file that is to be
#'   processed
#' @family data preparation functions
#' @return NULL

prepare_prewrangled_capacity_factors_WEO2019 <- function(data) {
  # WEO2019 start year should be the release year
  start_year <- 2019

  # WEO2019 end year in raw data always 2040
  end_year <- 2040

  # incompatible with 2017 and 2018 datasets
  data_has_expected_columns <- all(
    c(
      "Source", "Indicator", "Sector", "Units", "Scenario", "ScenarioGeography",
      "Technology", "Sub_Technology", as.character(end_year)
    ) %in% colnames(data)
  )
  stopifnot(data_has_expected_columns)

  hours_to_year <- 24 * 365

  data <- data %>%
    dplyr::filter(
      .data$Sector == "Power",
      .data$Technology != "Total capacity"
    )

  data <- data %>%
    dplyr::filter(
      (.data$Technology != "Renewables" & is.na(.data$Sub_Technology)) |
        (.data$Technology == "Renewables" & !is.na(.data$Sub_Technology))
    )

  data <- data %>%
    dplyr::mutate(
      Technology = dplyr::if_else(
        .data$Technology == "Renewables" & .data$Sub_Technology == "Hydro",
        "Hydro",
        .data$Technology
      )
    )

  data <- data %>%
    dplyr::select(
      .data$Source, .data$Indicator, .data$Sector, .data$Units, .data$Scenario,
      .data$ScenarioGeography, .data$Technology, dplyr::starts_with("20")
    ) %>%
    dplyr::group_by(
      .data$Source, .data$Indicator, .data$Sector, .data$Units, .data$Scenario,
      .data$ScenarioGeography, .data$Technology
    ) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::starts_with("20"),
        ~ sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::ungroup()

  capacity <- data %>%
    dplyr::filter(.data$Indicator == "Capacity") %>%
    dplyr::rename(units = .data$Units) %>%
    tidyr::pivot_longer(
      cols = tidyr::starts_with("20"),
      names_to = "year",
      values_to = "capacity"
    ) %>%
    dplyr::mutate(
      year = as.integer(.data$year)
    ) %>%
    dplyr::select(
      .data$Source, .data$Scenario, .data$ScenarioGeography, .data$Sector,
      .data$Technology, .data$year, .data$units, .data$capacity
    )

  generation <- data %>%
    dplyr::filter(.data$Indicator == "Generation") %>%
    dplyr::rename(units = .data$Units) %>%
    tidyr::pivot_longer(
      cols = tidyr::starts_with("20"),
      names_to = "year",
      values_to = "generation"
    ) %>%
    dplyr::mutate(
      year = as.integer(.data$year)
    ) %>%
    dplyr::select(
      .data$Source, .data$Scenario, .data$ScenarioGeography, .data$Sector,
      .data$Technology, .data$year, .data$units, .data$generation
    ) %>%
    dplyr::mutate(
      generation = .data$generation * 1000 / .env$hours_to_year,
      units = "GW"
    )

  capacity_factors <- generation %>%
    dplyr::inner_join(
      capacity,
      by = c(
        "Source", "Scenario", "ScenarioGeography", "Sector", "Technology", "units", "year"
      )
    ) %>%
    dplyr::distinct_all() %>%
    tidyr::complete(
      year = seq(.env$start_year, .env$end_year),
      tidyr::nesting(
        !!!rlang::syms(
          c("Source", "Scenario", "ScenarioGeography", "Sector", "Technology", "units")
        )
      )
    ) %>%
    dplyr::arrange(
      .data$Source, .data$Scenario, .data$ScenarioGeography, .data$Sector,
      .data$Technology, .data$units, .data$year
    ) %>%
    dplyr::group_by(
      .data$Source, .data$Scenario, .data$ScenarioGeography, .data$Sector,
      .data$Technology, .data$units
    ) %>%
    dplyr::mutate(
      # interpolate missing values using linear interpolation to avoid problems
      # with unrealistic lower/upper bounds
      capacity = zoo::na.approx(object = .data$capacity),
      generation = zoo::na.approx(object = .data$generation)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$year >= .env$start_year)

  capacity_factors <- capacity_factors %>%
    dplyr::mutate(capacity_factor = .data$generation / .data$capacity) %>%
    # if both capacity and generation are 0, we get capacity factor NaN. Until
    # we have clarity on how to best handle this, we assume capacity factor 0
    # in such a a case
    dplyr::mutate(
      capacity_factor = dplyr::if_else(
        is.na(.data$capacity_factor),
        0,
        .data$capacity_factor
      )
    ) %>%
    dplyr::rename(
      source = .data$Source,
      scenario = .data$Scenario,
      # TODO: check if region and region_2dii are somehow required. see prior version
      scenario_geography = .data$ScenarioGeography,
      ald_sector = .data$Sector,
      technology = .data$Technology
    ) %>%
    dplyr::mutate(
      technology = dplyr::case_when(
        .data$technology == "Coal" ~ "CoalCap",
        .data$technology == "Oil" ~ "OilCap",
        .data$technology == "Natural gas" ~ "GasCap",
        .data$technology == "Hydro" ~ "HydroCap",
        .data$technology == "Nuclear" ~ "NuclearCap",
        .data$technology == "Renewables" ~ "RenewablesCap",
        TRUE ~ .data$technology
      )
    ) %>%
    dplyr::mutate(
      scenario_geography = dplyr::if_else(
        .data$scenario_geography == "World",
        "Global",
        .data$scenario_geography
      )
    )

  capacity_factors_has_nas <- any(is.na(capacity_factors$capacity_factor))
  if (capacity_factors_has_nas) {
    stop("Data must not contain capacity factors with NA values.", call. = FALSE)
  }

  capacity_factors_out_of_bounds <- min(
    capacity_factors$capacity_factor,
    na.rm = TRUE
  ) < 0 |
    max(capacity_factors$capacity_factor, na.rm = TRUE) > 1
  if (capacity_factors_out_of_bounds) {
    stop(
      "Capacity factors with values below 0 or greater than 1 in data. This is
      not logically possible. Please check input data and fix.",
      call. = FALSE
    )
  }

  capacity_factors <- capacity_factors %>%
    dplyr::select(
      .data$scenario, .data$scenario_geography, .data$technology, .data$year,
      .data$capacity_factor
    )

  output_has_expected_columns <- all(
    c(
      "scenario", "scenario_geography", "technology", "year", "capacity_factor"
    ) %in% colnames(capacity_factors)
  )
  stopifnot(output_has_expected_columns)

  capacity_factors <- capacity_factors %>%
    dplyr::mutate(ald_sector = "Power") %>%
    # Power is the only currently included sector
    remove_incomplete_sectors() %>%
    dplyr::select(-.data$ald_sector) %>%
    dplyr::mutate(scenario = paste("WEO2019", .data$scenario, sep = "_"))

  capacity_factors
}

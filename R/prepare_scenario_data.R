#' This function reads scenario data in the form of data as found in the dropbox
#' under "Processed Data" and wrangles it to fit the required format for the
#' stress test.
#'
#' @param data Tibble that contains the scenario data file that is to be
#'   processed
#' @param start_year Numeric holding start year.
#' @family data preparation functions
#' @export
prepare_scenario_data <- function(data, start_year) {
  data_has_expected_columns <- all(
    c(
      "Source", "Technology", "ScenarioGeography", "Sector", "Units",
      "Indicator", "Scenario", "Sub_Technology", "Year", "Direction", "mktFSRatio", "techFSRatio",
      "FairSharePerc"
    ) %in% colnames(data)
  )
  stopifnot(data_has_expected_columns)

  # due to inconsistencies in the raw data across sources, we need to filter for
  # other Indicators in IEA scenarios than in GECO scenarios at least up until
  # WEO 2021 and GECO 2021. Please review once new scenarios are available
  data <- data %>%
    dplyr::filter(
      (stringr::str_detect(.data$Source, "WEO2021") & .data$Indicator %in% c("Capacity", "Energy Supply", "Production", "Sales")) |
        (stringr::str_detect(.data$Source, "GECO2021") & .data$Indicator %in% c("Capacity", "Production", "Sales"))
    ) %>%
    # for GECO2021, we can only cover Automotive, as we do not have any price or
    # cost information for other sectors
    dplyr::filter(
      !(stringr::str_detect(.data$Source, "GECO2021") & .data$Sector != "Automotive")
    ) %>%
    dplyr::filter(
      !(.data$Technology == "RenewablesCap" & !is.na(.data$Sub_Technology)) # THIS NEEDS TO BE INVESTIGATED AS SUBTECHNOLOGIES CURRENTLY ALWAYS EMPTY
    ) %>%
    dplyr::select(
      -c(
        .data$Sub_Technology, .data$Indicator, .data$mktFSRatio, .data$techFSRatio
      )
    ) %>%
    dplyr::rename(
      scenario_source = .data$Source,
      scenario_geography = .data$ScenarioGeography,
      scenario = .data$Scenario,
      ald_sector = .data$Sector,
      units = .data$Units,
      technology = .data$Technology,
      year = .data$Year,
      direction = .data$Direction,
      fair_share_perc = .data$FairSharePerc
    ) %>%
    dplyr::relocate(
      .data$scenario_source, .data$scenario_geography, .data$scenario,
      .data$ald_sector, .data$units, .data$technology, .data$year,
      .data$direction, .data$fair_share_perc
    ) %>%
    dplyr::mutate(
      scenario = stringr::str_c(.data$scenario_source, .data$scenario, sep = "_")
    ) %>%
    dplyr::distinct_all()

  # We can only use scenario x scenario_geography combinations that do not have
  # NAs on any not nullable columns. We currently  use STEPS, SDS, APS and NZE_2050, thus for
  # now only affected scenario x scenario_geography combinations in those sectors
  # are removed. Rows are removed from all scenarios as soon as scenario_geography
  # is affected on any of the scenarios.
  NA_geos <- data %>%
    dplyr::filter(
      .data$scenario %in% c(
        "WEO2021_STEPS", "WEO2021_SDS", "WEO2021_NZE_2050", "WEO2021_APS",
        "WEO2020_SPS", "GECO2021_CurPol", "GECO2021_1.5C-Unif", "GECO2021_NDC-LTS"
      )
    ) %>%
    dplyr::filter_all(dplyr::any_vars(is.na(.))) %>%
    dplyr::distinct(.data$scenario_source, .data$scenario_geography, .data$ald_sector)

  data <- data %>%
    dplyr::anti_join(NA_geos, by = c("scenario_source", "scenario_geography", "ald_sector"))

  # removing sectors that are not supported by stress testing
  p4i_p4b_sector_technology_lookup <- p4i_p4b_sector_technology_lookup()

  data <- data %>%
    dplyr::filter(.data$ald_sector %in% unique(p4i_p4b_sector_technology_lookup$sector_p4i))

  data <- remove_incomplete_sectors(data)

  data <- data %>%
    dplyr::select(-.data$scenario_source)

  return(data)
}


#' This function reads  NGFS raw scenario data as found in the dropbox
#' under "Processed Data" and wrangles it to fit the required format for the
#' usual scenario analysis input routine.
#'
#' @param data Tibble that contains the scenario data file that is to be
#'   processed
#' @family data preparation functions
#' @export

preprepare_ngfs_scenario_data <- function(data) {
  start_year <- 2021

  data <- data %>%
    dplyr::mutate(scenario = .data$Scenario) %>%
    dplyr::mutate(
      scenario = dplyr::case_when(
        .data$scenario == "Nationally Determined Contributions (NDCs)" ~ "NDC",
        .data$scenario == "Below 2 C" ~ "B2DS",
        .data$scenario == "Delayed transition" ~ "DT",
        .data$scenario == "Current Policies" ~ "CP",
        .data$scenario == "Divergent Net Zero" ~ "DN0",
        .data$scenario == "Net Zero 2050" ~ "NZ2050",
        TRUE ~ .data$scenario
      ),
      scenario_geography = dplyr::case_when(
        .data$Region == "World" ~ "Global",
        TRUE ~ .data$Region
      ),
      sector = dplyr::case_when(
        .data$category_b == "Oil" ~ "Oil&Gas",
        .data$category_b == "Gas" ~ "Oil&Gas",
        .data$category_b == "Coal" ~ "Coal",
        TRUE ~ "Power"
      ),
      technology = dplyr::case_when(
        .data$category_b == "Oil" ~ "Oil",
        .data$category_b == "Gas" ~ "Gas",
        .data$category_b == "Coal" ~ "Coal",
        .data$category_b == "Electricity" & .data$category_c == "Coal" ~ "CoalCap",
        .data$category_b == "Electricity" & .data$category_c == "Gas" ~ "GasCap",
        .data$category_b == "Electricity" & .data$category_c == "Hydro" ~ "HydroCap",
        .data$category_b == "Electricity" & .data$category_c == "Nuclear" ~ "NuclearCap",
        .data$category_b == "Electricity" & .data$category_c == "Oil" ~ "OilCap",
        .data$category_b == "Electricity" & .data$category_c == "Solar" ~ "RenewablesCap",
        .data$category_b == "Electricity" & .data$category_c == "Geothermal" ~ "RenewablesCap",
        .data$category_b == "Electricity" & .data$category_c == "Biomass" ~ "RenewablesCap",
        .data$category_b == "Electricity" & .data$category_c == "Wind" ~ "RenewablesCap",
        TRUE ~ .data$category_c
      ),
      indicator = dplyr::if_else(
        .data$sector == "Power", "Capacity", "Production"
      ),
      source = paste("NGFS", start_year, sep = ""),
      model = dplyr::case_when(
        .data$Model == "GCAM 5.3+ NGFS" ~ "GCAM",
        .data$Model == "REMIND-MAgPIE 3.0-4.4" ~ "REMIND",
        .data$Model == "MESSAGEix-GLOBIOM 1.1-M-R12" ~ "MESSAGE",
        TRUE ~ .data$Model
      )
    ) %>%
    dplyr::rename(units = .data$Unit) %>%
    dplyr::select(-c(.data$Model, .data$Variable, .data$Scenario, .data$category_c, .data$category_a, .data$category_b, .data$Region))


  combine_renewables_cap <- data %>%
    dplyr::filter(.data$technology == "RenewablesCap") %>%
    dplyr::group_by(.data$year, .data$technology, .data$scenario_geography, .data$model, .data$scenario) %>%
    dplyr::mutate(value = sum(.data$value)) %>%
    unique()

  delete_renewables <- data %>% dplyr::filter(!.data$technology == "RenewablesCap")

  data <- dplyr::full_join(combine_renewables_cap, delete_renewables) %>%
    tidyr::unite("scenario", c(.data$model, .data$scenario), sep = "_") %>%
    dplyr::mutate(scenario = paste("NGFS2021", .data$scenario, sep = "_"))
}

#' Interpolate values in a dataset, by year.
#'
#' @param data An input dataset. Must contain the columns `year` and `value`.
#' @param ... Other grouping variables. `value` will be interpolated for each
#'   group.
#'
#' @return A dataset with the column `value` interpolated linearly against the
#'   column `year`.
#'
#' @export

interpolate_yearly <- function(data, ...) {
  data %>%
    dplyr::group_by(...) %>%
    tidyr::complete(year = tidyr::full_seq(.data$year, 1)) %>%
    dplyr::mutate(
      value = zoo::na.approx(.data$value, .data$year, na.rm = FALSE)
    ) %>%
    dplyr::ungroup()
}

#' Add market share columns to a scenario dataset
#'
#' Calculates and adds market share values (ie. technology market-share ratio
#' and sector market-share percentage) to a scenario dataset. A reference
#' start-year must be provided.
#'
#' @param data A scenario dataset, in this case NGFS 2021.
#' @param start_year The baseline year, against which the technology- and
#'   sector- market shares will be calculated. Note: At the start year, tmsr = 1
#'   and smsp =0 respectively.
#'
#' @return A scenario dataset, with the new columns `tmsr` and `smsp`.
#'
#' @export
add_market_share_columns <- function(data, start_year) {
  old_groups <- dplyr::groups(data)
  data <- dplyr::ungroup(data)

  data %>%
    add_technology_fair_share_ratio() %>%
    add_market_fair_share_percentage() %>%
    dplyr::group_by(!!!old_groups)
}

add_technology_fair_share_ratio <- function(data) {
  data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!!rlang::syms(c(common_fs_groups(), "technology"))) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::mutate(tmsr = (.data$value - dplyr::first(.data$value)) / dplyr::first(.data$value)) %>%
    dplyr::ungroup()
}

add_market_fair_share_percentage <- function(data) {
  data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!!rlang::syms(c(common_fs_groups(), "year"))) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::mutate(sector_total_by_year = sum(.data$value)) %>%
    dplyr::group_by(!!!rlang::syms(c(common_fs_groups(), "technology"))) %>%
    dplyr::mutate(
      smsp = (.data$value - dplyr::first(.data$value)) /
        dplyr::first(.data$sector_total_by_year),
      sector_total_by_year = NULL
    ) %>%
    dplyr::ungroup()
}

common_fs_groups <- function() {
  c("scenario", "sector", "scenario_geography")
}

format_p4i <- function(data, green_techs) {
  crucial_names <- c(
    "source",
    "scenario",
    "scenario_geography",
    "sector",
    "technology",
    "indicator",
    "units",
    "year",
    "tmsr",
    "smsp"
  )

  check_crucial_names(data, crucial_names)

  data %>%
    dplyr::mutate(Sub_Technology = NA) %>% # this column should be dropped from PACTA
    dplyr::mutate(
      direction = dplyr::if_else(.data$technology %in% .env$green_techs, "increasing", "declining"),
      fair_share_perc = dplyr::if_else(.data$direction == "declining", .data$tmsr, .data$smsp)
    ) %>%
    dplyr::select(
      .data$scenario_geography,
      .data$scenario,
      ald_sector = .data$sector,
      .data$technology,
      .data$units,
      .data$year,
      .data$direction,
      .data$fair_share_perc
    )
}

#' Check if a named object contains expected names
#'
#' Based on fgeo.tool::check_crucial_names()
#'
#' @param x A named object.
#' @param expected_names String; expected names of `x`.
#'
#' @return Invisible `x`, or an error with informative message.
#'
#' @examples
#' x <- c(a = 1)
#' check_crucial_names(x, "a")
#' try(check_crucial_names(x, "bad"))
#' @noRd
check_crucial_names <- function(x, expected_names) {
  stopifnot(rlang::is_named(x))
  stopifnot(is.character(expected_names))

  ok <- all(unique(expected_names) %in% names(x))
  if (!ok) {
    abort_missing_names(sort(setdiff(expected_names, names(x))))
  }

  invisible(x)
}

abort_missing_names <- function(missing_names) {
  nms <- glue::glue_collapse(missing_names, sep = ", ", last = ", and ")
  abort(glue::glue("Must have missing names:\n{nms}."), class = "missing_names")
}

#### IPR Scenario Analysis Function
#### Prepares Scenario Analysis Input for IPR using the usual routine
prepare_IPR_scenario_data <- function(data) {
  ### Creating a technology column

  data$technology <- ifelse(data$Sector == "Power", paste(data$Sub_variable_class_2, data$Sector, sep = "_"), data$Sub_variable_class_1)


  ### Renaming technologies and Sector

  data <- data %>%
    dplyr::rename(ald_sector = Sector) %>%
    dplyr::mutate(technology = .data$technology) %>%
    dplyr::mutate(
      technology = dplyr::case_when(
        .data$technology == "Oil" ~ "Oil",
        .data$technology == "Coal" ~ "Coal",
        .data$technology == "Natural gas" ~ "Gas",
        .data$technology == "Coal_Power" ~ "CoalCap",
        .data$technology == "Natural gas_Power" ~ "GasCap",
        .data$technology == "Oil_Power" ~ "OilCap",
        .data$technology == "Nuclear_Power" ~ "NuclearCap",
        .data$technology == "Hydro_Power" ~ "HydroCap",
        .data$technology == "Biomass_Power" ~ "BiomassCap", ### Is this the same as Biomass?
        .data$technology == "Offshore wind_Power" ~ "OffWindCap",
        .data$technology == "Onshore wind_Power" ~ "OnWindCap",
        .data$technology == "Solar_Power" ~ "SolarCap"
      ),
      ald_sector = dplyr::case_when(
        .data$technology == "Oil" ~ "Oil&Gas",
        .data$technology == "Gas" ~ "Oil&Gas",
        .data$technology == "Coal" ~ "Coal",
        .data$technology == "CoalCap" ~ "Power",
        .data$technology == "GasCap" ~ "Power",
        .data$technology == "OilCap" ~ "Power",
        .data$technology == "NuclearCap" ~ "Power",
        .data$technology == "HydroCap" ~ "Power",
        .data$technology == "BiomassCap" ~ "Power",
        .data$technology == "OffWindCap" ~ "Power",
        .data$technology == "OnWindCap" ~ "Power",
        .data$technology == "SolarCap" ~ "Power"
      ),
      Scenario = dplyr::case_when(
        .data$Scenario == "RPS" ~ "IPR2021_RPS",
        .data$Scenario == "FPS" ~ "IPR2021_FPS"
      )
    )

  ### Renaming Region WORLD to Global

  data <- data %>%
    dplyr::mutate(Region = ifelse(Region == "WORLD", "Global", Region))

  ### Deleting all NAs, NAs exist because the current data still has data that we are currently not using, like hydrogen and Coal with CCS

  data <- data[!(is.na(data$ald_sector)), ]

  ### further deleting unnecessary columns

  data <- dplyr::select(data, -c("Variable_class", "Sub_variable_class_1", "Sub_variable_class_2"))

  ### renaming column names

  colnames(data)[colnames(data) == "Scenario"] <- "scenario"
  colnames(data)[colnames(data) == "Region"] <- "scenario_geography"
  colnames(data)[colnames(data) == "Units"] <- "units"

  ### creating Renewablescap

  combine_Renewablecap <- data[data$technology == "OffWindCap" | data$technology == "OnWindCap" | data$technology == "SolarCap" | data$technology == "BiomassCap", ]

  combine_Renewablecap <- combine_Renewablecap %>%
    dplyr::group_by(.data$scenario_geography, .data$scenario, .data$ald_sector, .data$units, .data$year) %>%
    dplyr::summarize(value = sum(.data$value)) %>%
    dplyr::ungroup()

  combine_Renewablecap$technology <- "RenewablesCap"

  ### binding RenewablesCap with main data

  data <- rbind(data, combine_Renewablecap)

  ### Deleting Offwind, Onwind, Solar and Biomass, to avoid double counting
  data <- data[!(data$technology == "OffWindCap" | data$technology == "OnWindCap" | data$technology == "SolarCap" | data$technology == "BiomassCap"), ]

  ### Calculating TMSR

  start_year <- 2021
  data$year <- as.numeric(as.character(data$year))
  data <- data[!(data$year < start_year), ]

  data <- data %>%
    dplyr::group_by(.data$scenario_geography, .data$scenario, .data$ald_sector, .data$units, .data$technology) %>%
    dplyr::arrange(data$year, .by_group = TRUE) %>%
    dplyr::mutate(tmsr = (.data$value - dplyr::first(.data$value)) / dplyr::first(.data$value))


  ### Calculating SMSP

  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$scenario_geography, .data$scenario, .data$ald_sector, .data$units, .data$year) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::mutate(sector_total_by_year = sum(.data$value)) %>%
    dplyr::group_by(.data$scenario_geography, .data$scenario, .data$ald_sector, .data$units, .data$technology) %>%
    dplyr::mutate(
      smsp = (.data$value - dplyr::first(.data$value)) /
        dplyr::first(.data$sector_total_by_year),
      sector_total_by_year = NULL
    ) %>%
    dplyr::ungroup()


  ### Green Techs, Direction and FairSharePerc
  ### Defines direction of technology based on whether its considered a green technology

  green_techs <- c("RenewablesCap", "HydroCap", "NuclearCap", "SolarCap", "OffWindCap", "OnWindCap", "BiomassCap")

  data <- data %>%
    dplyr::mutate(
      direction = dplyr::if_else(.data$technology %in% green_techs, "increasing", "declining"),
      fair_share_perc = dplyr::if_else(.data$direction == "declining", .data$tmsr, .data$smsp),
      tmsr = NULL,
      smsp = NULL,
      value = NULL
    )

  data <- data[, c(
    "scenario_geography", "scenario", "ald_sector", "technology", "units", "year",
    "direction", "fair_share_perc"
  )]
}

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
      !(.data$Technology == "RenewablesCap" & !is.na(.data$Sub_Technology))     #THIS NEEDS TO BE INVESTIGATED AS SUBTECHNOLOGIES CURRENTLY ALWAYS EMPTY
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
  data <- data %>%
    dplyr::filter(.data$ald_sector %in% unique(p4i_p4b_sector_technology_lookup$sector_p4i))
  
  data <- remove_incomplete_sectors(data)
  
  data <- data %>%
    dplyr::select(-.data$scenario_source)
  
  return(data)
}

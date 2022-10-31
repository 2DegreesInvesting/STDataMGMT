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
      "Indicator", "Scenario", "Sub_Technology", "Year", "AnnualvalIEAtech",
      "refvalIEAtech", "refvalIEAsec", "Direction", "mktFSRatio", "techFSRatio",
      "FairSharePerc"
    ) %in% colnames(data)
  )
  stopifnot(data_has_expected_columns)

  # due to inconsistencies in the raw data across sources, we need to filter for
  # other Indicators in IEA scenarios than in GECO scenarios at least up until
  # WEO 2020 and GECO 2019. Please review once new scenarios are available
  data <- data %>%
    dplyr::filter(
      (stringr::str_detect(.data$Source, "WEO") & .data$Indicator %in% c("Capacity", "Production", "Sales")) |
        # Energy supply should likely be transformed to the common production units first...
        (stringr::str_detect(.data$Source, "WEO2021") & .data$Indicator %in% c("Capacity", "Energy Supply", "Production", "Sales")) |
        (stringr::str_detect(.data$Source, "ETP") & .data$Indicator %in% c("Capacity", "Production", "Sales")) |
        (stringr::str_detect(.data$Source, "GECO2021") & .data$Indicator %in% c("Capacity", "Production", "Sales")) |
        # ADO4977 - for GECO up until 2019, there is no distinction and indicators are
        # prepared to the appropriate unit used in PACTA
        (stringr::str_detect(.data$Source, "GECO2019") & is.na(.data$Indicator))
    ) %>%
    # for GECO2019, we can only cover Automotive, as we do not have any price or
    # cost information for other sectors
    dplyr::filter(
      !(stringr::str_detect(.data$Source, "GECO2019") & .data$Sector != "Automotive")
    ) %>%
    dplyr::filter(
      !(.data$Technology == "RenewablesCap" & !is.na(.data$Sub_Technology))
    ) %>%
    dplyr::filter(
      !(.data$Source %in% c("GP_ER_2015", "BNEF2017", "WEO2017", "WEO2018", "SBTI"))
    ) %>%
    dplyr::select(
      -c(
        .data$Sub_Technology, .data$Indicator, .data$AnnualvalIEAtech,
        .data$refvalIEAtech, .data$refvalIEAsec, .data$mktFSRatio, .data$techFSRatio
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
      scenario = stringr::str_replace(.data$scenario, "NPSRTS", "NPS")
    ) %>%
    dplyr::mutate(
      scenario = stringr::str_c(.data$scenario_source, .data$scenario, sep = "_")
    ) %>%
    dplyr::filter(!(.data$scenario_source == "ETP2017" & .data$ald_sector == "Power")) %>%
    dplyr::distinct_all()

  # We can only use scenario x scenario_geography combinations that do not have
  # NAs on any not nullable columns. We currently only use SPS and SDS, thus for
  # now only affected scenario x scenario_geography combinations in those sectors
  # are removed. Rows are removed from all scenarios as soon as scenario_geography
  # is affected on any of the scenarios.
  NA_geos <- data %>%
    dplyr::filter(
      .data$scenario %in% c(
        "ETP2017_NPS", "ETP2017_SDS", "WEO2019_SDS", "WEO2019_SPS", "WEO2020_SDS",
        "WEO2020_SPS", "GECO2019_ref", "GECO2019_1.5c", "GECO2019_2c_m"
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

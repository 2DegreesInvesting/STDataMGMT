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
        .data$Model == "REMIND-MAgPIE 3.0-4.4" ~ "REMIND-MAgPIE",
        .data$Model == "MESSAGEix-GLOBIOM 1.1-M-R12" ~ "MESSAGEix - GLOBIUM",
        TRUE ~ .data$Model
      )
    ) %>%
    dplyr::rename(scenario_geography = .data$Region, units = .data$Unit) %>%
    dplyr::select(-c(.data$Model, .data$Variable, .data$Scenario, .data$category_c, .data$category_a, .data$category_b))


  combine_renewables_cap <- data %>%
    dplyr::filter(.data$technology == "RenewablesCap") %>%
    dplyr::group_by(.data$year, .data$technology, .data$scenario_geography, .data$model, .data$scenario) %>%
    dplyr::mutate(value = sum(.data$value)) %>%
    unique()

  delete_renewables <- data %>% dplyr::filter(!.data$technology == "RenewablesCap")

  data <- dplyr::full_join(combine_renewables_cap, delete_renewables)
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
    dplyr::group_by(!!!rlang::syms(c(common_fs_groups(), "technology", "model"))) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::mutate(tmsr = (.data$value - dplyr::first(.data$value)) / dplyr::first(.data$value)) %>%
    dplyr::ungroup()
}

add_market_fair_share_percentage <- function(data) {
  data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!!rlang::syms(c(common_fs_groups(), "year", "model"))) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::mutate(sector_total_by_year = sum(.data$value)) %>%
    dplyr::group_by(!!!rlang::syms(c(common_fs_groups(), "technology", "model"))) %>%
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
    "model",
    "units",
    "year",
    "tmsr",
    "smsp"
  )

  check_crucial_names(data, crucial_names)

  data %>%
    dplyr::mutate(Sub_Technology = NA) %>% # this column should be dropped from PACTA
    dplyr::mutate(
      Direction = dplyr::if_else(.data$technology %in% .env$green_techs, "increasing", "declining"),
      FairSharePerc = dplyr::if_else(.data$Direction == "declining", .data$tmsr, .data$smsp)
    ) %>%
    dplyr::select(
      Source = .data$source,
      ScenarioGeography = .data$scenario_geography,
      Model = .data$model,
      Scenario = .data$scenario,
      Sector = .data$sector,
      Technology = .data$technology,
      .data$Sub_Technology,
      Indicator = .data$indicator,
      Units = .data$units,
      Year = .data$year,
      techFSRatio = .data$tmsr,
      mktFSRatio = .data$smsp,
      .data$Direction,
      .data$FairSharePerc
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

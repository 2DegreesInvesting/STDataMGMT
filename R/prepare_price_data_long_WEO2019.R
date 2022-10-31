#' This function reads in multiple price data files from WEO2019 and wrangles
#' it to fit the format to be used in the transition risk stress test work flow.
#'
#' @param input_data_fossil_fuel Tibble containing the raw price data for fossil
#'   fuels
#' @param input_data_power Tibble containing the raw price/LCOE data for power
#'
#' @family data preparation functions
#'
#' @export
prepare_price_data_long_WEO2019 <- function(input_data_fossil_fuel,
                                            input_data_power) {
  # the WEO 2019 raw data has values for 2018, but not for 2019. So the first year
  # must be 2018, if data prior to 2025/30 (next value) are to be interpolated.
  first_year <- 2018

  fossil_fuels_data_has_expected_columns <- all(
    c(
      "source", "sector", "unit", "scenario_geography", "scenario"
    ) %in% colnames(input_data_fossil_fuel)
  )
  stopifnot(fossil_fuels_data_has_expected_columns)

  fossil_fuels_data <- input_data_fossil_fuel %>%
    tidyr::pivot_longer(
      cols = tidyr::starts_with("20"),
      names_to = "year",
      values_to = "price"
    ) %>%
    dplyr::mutate(
      year = as.numeric(.data$year)
    ) %>%
    dplyr::filter(.data$year >= .env$first_year)

  fossil_fuels_data <- fossil_fuels_data %>%
    # ADO 1192: technology is wrongly called sector in raw data. rename
    dplyr::rename(technology = .data$sector) %>%
    dplyr::mutate(
      technology = dplyr::case_when(
        .data$technology == "Crude oil" ~ "Oil",
        .data$technology == "Natural gas" ~ "Gas",
        TRUE ~ .data$technology
      )
    ) %>%
    dplyr::mutate(
      sector = dplyr::if_else(
        .data$technology == "Coal",
        "Coal",
        "Oil&Gas"
      ),
      scenario_geography = dplyr::if_else(
        .data$scenario_geography == "European Union",
        "EU",
        .data$scenario_geography
      )
    ) %>%
    dplyr::mutate(
      indicator = "price"
    )

  # ADO 1192 - for some technologies, there are no global price data.
  # approximate with simple mean based on all other given regions
  fossil_fuels_data_global <- fossil_fuels_data %>%
    dplyr::filter(.data$technology %in% c("Gas", "Coal")) %>%
    dplyr::group_by(
      .data$source, .data$scenario, .data$sector, .data$technology, .data$year,
      .data$unit, .data$indicator
    ) %>%
    dplyr::summarise(
      price = mean(.data$price, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(scenario_geography = "Global")

  fossil_fuels_data <- fossil_fuels_data %>%
    dplyr::bind_rows(fossil_fuels_data_global)

  power_data_has_expected_columns <- all(
    c(
      "source", "scenario", "region", "technology", "indicator", "unit"
    ) %in% colnames(input_data_power)
  )
  stopifnot(power_data_has_expected_columns)

  # NOTE: WEO 2019 does not provide LCOE for SDS. This was approximated by using
  # the ratio between SPS and SDS based on WEO 2020 and applying that factor to
  # WEO 2019 SPS LCOE data to get a rough estimate for 2019 SDS LCOE data
  power_data <- input_data_power %>%
    tidyr::pivot_longer(
      cols = tidyr::starts_with("20"),
      names_to = "year",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      year = as.numeric(.data$year)
    ) %>%
    dplyr::filter(.data$year >= .env$first_year)

  power_data <- power_data %>%
    dplyr::filter(.data$indicator == "LCOE") %>%
    dplyr::rename(
      scenario_geography = .data$region,
      price = .data$value
    ) %>%
    dplyr::mutate(
      technology = dplyr::case_when(
        .data$technology == "Nuclear" ~ "NuclearCap",
        .data$technology == "Coal" ~ "CoalCap",
        .data$technology == "Gas CCGT" ~ "GasCap",
        TRUE ~ "RenewablesCap"
      )
    ) %>%
    dplyr::mutate(
      scenario_geography = dplyr::if_else(
        .data$scenario_geography == "European Union",
        "EU",
        .data$scenario_geography
      )
    ) %>%
    dplyr::mutate(
      sector = "Power"
    ) %>%
    dplyr::group_by(
      .data$source, .data$scenario, .data$scenario_geography, .data$sector,
      .data$technology, .data$unit, .data$year, .data$indicator
    ) %>%
    # ado 1192 - summarise so that every technology only has one row left per
    # combination. Because of combining multiple wind and solar techs from the
    # raw data into "RenewablesCap" in the processed data, this is not initially
    # the case
    dplyr::summarise(
      price = mean(.data$price, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(names(fossil_fuels_data))

  missing_power_data <- power_data %>%
    dplyr::filter(.data$technology %in% c("GasCap", "RenewablesCap")) %>%
    # ADO 1192 - use gascap and renwablescap data for oilcap and hydrocap
    # as a placeholder until IEA replies
    dplyr::mutate(
      technology = dplyr::case_when(
        .data$technology == "GasCap" ~ "OilCap",
        .data$technology == "RenewablesCap" ~ "HydroCap",
        TRUE ~ .data$technology
      )
    )

  power_data <- power_data %>%
    dplyr::bind_rows(missing_power_data)

  # ADO 1192 - there are no global price data for any of the power technologies
  # approximate with simple mean based on all other given regions
  power_data_global <- power_data %>%
    dplyr::group_by(
      .data$source, .data$scenario, .data$year, .data$sector, .data$technology,
      .data$unit, .data$indicator
    ) %>%
    dplyr::summarise(
      price = mean(.data$price, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(scenario_geography = "Global")

  power_data <- power_data %>%
    dplyr::bind_rows(power_data_global)

  data <- fossil_fuels_data %>%
    dplyr::bind_rows(power_data) %>%
    dplyr::relocate(
      .data$source, .data$scenario, .data$scenario_geography, .data$sector,
      .data$technology, .data$indicator, .data$unit, .data$year, .data$price
    )

  min_year <- min(data$year, na.rm = TRUE)
  max_year <- max(data$year, na.rm = TRUE)

  data <- data %>%
    tidyr::complete(
      year = seq(.env$min_year, .env$max_year),
      tidyr::nesting(
        !!!rlang::syms(
          c(
            "source", "scenario", "scenario_geography", "sector", "technology",
            "indicator", "unit"
          )
        )
      )
    ) %>%
    dplyr::arrange(
      .data$source, .data$scenario, .data$scenario_geography, .data$sector,
      .data$technology, .data$indicator, .data$unit, .data$year
    )

  data <- data %>%
    dplyr::group_by(
      .data$source, .data$scenario, .data$scenario_geography, .data$sector,
      .data$technology, .data$indicator, .data$unit
    ) %>%
    dplyr::mutate(price = zoo::na.approx(object = .data$price)) %>%
    dplyr::ungroup()

  data_has_nas <- any(is.na(data$price))
  if (data_has_nas) {
    stop("Data must not contain prices with NA values.", call. = FALSE)
  }

  min_price_greater_equal_zero <- min(data$price, na.rm = TRUE) >= 0
  stopifnot(min_price_greater_equal_zero)

  return(data)
}

#' This function reads in multiple price data files from WEO2021 and wrangles
#' it to fit the format to be used in the transition risk stress test work flow.
#'
#' @param input_data_fossil_fuel Tibble containing the raw price data for fossil
#'   fuels
#' @param input_data_power Tibble containing the raw price/LCOE data for power
#'
#' @family data preparation functions
#'
#' @export
prepare_price_data_long_WEO2021 <- function(input_data_fossil_fuel,
                                            input_data_power) {
  # the WEO 2020 raw data has values for 2019, but not for 2020. So the first year
  # must be 2019, if data prior to 2025 (next value) are to be interpolated.
  first_year <- 2020

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

  # map price units to AR production units
  # coal: tonnes of coal -> tonnes of coal (already correct)
  # Oil & Gas: barrel and Mbtu -> GJ
  fossil_fuels_data <- fossil_fuels_data %>%
    dplyr::mutate(
      price = dplyr::case_when(
        .data$unit == "usd/barrel" ~ 0.16 * .data$price,
        .data$unit == "usd/Mbtu" ~ 0.9478171203 * .data$price,
        TRUE ~ .data$price
      )
    ) %>%
    dplyr::mutate(
      unit = dplyr::case_when(
        .data$unit == "usd/barrel" ~ "GJ",
        .data$unit == "usd/Mbtu" ~ "GJ",
        TRUE ~ .data$unit
      )
    )

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

  power_data <- power_data %>%
    dplyr::group_by(
      .data$source, .data$technology, .data$unit, .data$scenario_geography,
      .data$scenario, .data$sector, .data$indicator
    ) %>%
    dplyr::arrange(
      .data$source, .data$technology, .data$unit, .data$scenario_geography,
      .data$scenario, .data$sector, .data$indicator, .data$year
    ) %>%
    tidyr::fill("price", .direction = "down") %>%
    dplyr::ungroup()


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

#' This function reads raw NGFS price data files from 2021 in and wrangles
#' it to fit the format to be used in the transition risk stress test work flow.
#'
#' @param input_data_fossil_fuels_ngfs A dataset containing prices for the fossil 
#' fuels 
#'
#' @family data preparation functions
#'
#' @export


prepare_price_data_long_NGFS2021  <- function(input_data_fossil_fuels_ngfs) {
  start_year <- 2021
  
  data <- input_data_fossil_fuels_ngfs %>%
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
      model = dplyr::case_when(
        .data$Model == "GCAM 5.3+ NGFS" ~ "GCAM",
        .data$Model == "REMIND-MAgPIE 3.0-4.4" ~ "REMIND",
        .data$Model == "MESSAGEix-GLOBIOM 1.1-M-R12" ~ "MESSAGE",
        TRUE ~ .data$Model
      ),
      sector = dplyr::case_when(
        .data$category_c == "Oil" ~ "Oil&Gas",
        .data$category_c == "Gas" ~ "Oil&Gas",
        .data$category_c == "Coal" ~ "Coal",
        TRUE ~ .data$category_c
      )) %>%
    dplyr::rename(scenario_geography = .data$Region, unit = .data$Unit, technology = .data$category_c, indicator = .data$category_a) %>%
    dplyr::select(-c(.data$Model, .data$Variable, .data$Scenario, .data$category_b))

  data <- data %>%
    dplyr::group_by(dplyr::across(-c(.data$year, .data$value))) %>%
    tidyr::complete(year = tidyr::full_seq(.data$year, 1)) %>%
    dplyr::mutate(
      value = zoo::na.approx(.data$value, .data$year, na.rm = FALSE)
    ) %>%
    dplyr::ungroup()
  
  data <- data %>% dplyr::filter(.data$year >= start_year)

  data_oil_gas <- data %>% dplyr::filter(.data$sector == "Oil&Gas") %>%
    dplyr::mutate(unit = "$/GJ")
  
  data_coal <- data %>% dplyr::filter(.data$sector == "Coal") %>% 
    dplyr::group_by(.data$year, .data$scenario_geography, .data$model, .data$scenario) %>% 
    dplyr::mutate(value = .data$value/ 0.03414368, unit = "$/tonnes")
  
  data <- dplyr::full_join(data_oil_gas, data_coal)

  data <- data %>% dplyr::rename(price = .data$value) %>%
    tidyr::unite("scenario", c(.data$model, .data$scenario), sep = "_")
  
}
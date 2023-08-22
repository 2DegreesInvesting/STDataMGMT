#' This function reads carbon price data sourced from the NGFS scenario selector
#' tool and prepares it for use in the r2dii.climate.stress.test package. Gaps
#' between years are interpolated.
#'
#' @param data Tibble that contains the raw ngfs scenario carbon price data file
#'   that is to be processed
#' @param start_year Numeric vector of length 1. Indicates the desired start
#'   year of the prepared ngfs price data set
#'
#' @family data preparation functions

prepare_ngfs_carbon_price <- function(data,
                                      start_year) {
  # for now end year is set to 2100. Think about surfacing this if necessary
  end_year <- 2100

  data_has_expected_columns <- all(
    c(
      "Model", "Scenario", "Region", "Variable", "Unit", as.character(end_year)
    ) %in% colnames(data)
  )
  stopifnot(data_has_expected_columns)

  # carbon tax is later interpolated on a yearly basis. This check ensures the
  # first year is outside of the interpolation range
  start_year_in_bounds <- any(
    c((start_year - 4):start_year) %in% colnames(data)
  )
  stopifnot(start_year_in_bounds)

  data <- data %>%
    dplyr::rename(
      model = .data$Model,
      scenario = .data$Scenario,
      scenario_geography = .data$Region,
      variable = .data$Variable,
      unit = .data$Unit
    ) %>%
    dplyr::mutate(
      scenario_geography = dplyr::if_else(
        .data$scenario_geography == "World",
        "Global",
        .data$scenario_geography
      )
    )

  ## I add here a no carbon tax model that serves as the default in the stress test
  no_carbon_tax <- tibble::tribble(
    ~model, ~scenario, ~scenario_geography, ~variable, ~unit, ~`2015`, ~`2020`, ~`2025`, ~`2030`, ~`2035`, ~`2040`, ~`2045`, ~`2050`,
    ~`2055`, ~`2060`, ~`2065`, ~`2070`, ~`2075`, ~`2080`, ~`2085`, ~`2090`, ~`2095`, ~`2100`,
    "no_carbon_tax", "no_carbon_tax", "Global", "Price|Carbon", "US$2010/t CO2", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  )

  data <- data %>% rbind(no_carbon_tax)

  data$`2025` <- ifelse(data$scenario == "NDC_Indonesia", 2, data$`2025`)
  data$`2030` <- ifelse(data$scenario == "NDC_Indonesia", NA, data$`2030`)
  data$`2035` <- ifelse(data$scenario == "NDC_Indonesia", 5, data$`2035`)

  data <- data %>%
    tidyr::pivot_longer(
      cols = tidyr::starts_with(c("20", "21")),
      names_to = "year",
      values_to = "carbon_tax"
    ) %>%
    dplyr::mutate(year = as.numeric(.data$year))

  data <- data %>%
    tidyr::complete(
      year = seq(min(.data$year), .env$end_year),
      tidyr::nesting(
        !!!rlang::syms(
          c("model", "scenario", "scenario_geography", "variable", "unit")
        )
      )
    ) %>%
    dplyr::arrange(
      .data$model, .data$scenario, .data$scenario_geography, .data$variable,
      .data$unit, .data$year
    ) %>%
    dplyr::group_by(
      .data$model, .data$scenario, .data$scenario_geography, .data$variable,
      .data$unit
    ) %>%
    dplyr::mutate(
      carbon_tax = dplyr::case_when(
        .data$scenario == "DN0" &
          .data$year >= 2025 ~
          zoo::na.approx(object = .data$carbon_tax),
        .data$scenario == "NDC" &
          .data$year >= 2025 ~
          zoo::na.approx(object = .data$carbon_tax),
        .data$scenario == "NZ2050" &
          .data$year >= 2025 ~
          zoo::na.approx(object = .data$carbon_tax),
        .data$scenario == "B2DS" &
          .data$year >= 2025 ~
          zoo::na.approx(object = .data$carbon_tax),
        .data$scenario == "DN0_Indonesia" &
          .data$year >= 2025 ~
          zoo::na.approx(object = .data$carbon_tax),
        .data$scenario == "B2DS_Indonesia" &
          .data$year >= 2025 ~
          zoo::na.approx(object = .data$carbon_tax),
        .data$scenario == "NZ2050_Indonesia" &
          .data$year >= 2025 ~
          zoo::na.approx(object = .data$carbon_tax),
        .data$scenario == "NDC_Indonesia" &
          .data$year >= 2025 ~
          zoo::na.approx(object = .data$carbon_tax),
        .data$scenario == "DT_Indonesia" &
          .data$year >= 2025 ~
          zoo::na.approx(object = .data$carbon_tax),
        TRUE ~ 0
      )
    ) %>%
    dplyr::ungroup()

  output_has_expected_columns <- all(
    c(
      "year", "model", "scenario", "scenario_geography", "variable", "unit",
      "carbon_tax"
    ) %in% colnames(data)
  )
  stopifnot(output_has_expected_columns)

  return(data)
}

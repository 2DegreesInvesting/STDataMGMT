#' This function reads in the long format prepared price data from IEA scenarios
#' that contain the LCOE for the power sector. It then derives implied market
#' prices. This is done by adding the average net profit margin of the 100
#' largest power companies we can find on top of the costs of the start year.
#' The absolute margin in that year is then added to all following years as well.
#' We adjust the price by calculating the cost changes for each technology and
#' scenario as lcoe_t0 / lcoe_t and applying the resulting cost factor to the
#' absolute profit margin in each year. This means that if costs increase, the
#' absolute average profit margin will be smaller over time, whereas falling
#' costs will increase the absolute profit margin over time.
#'
#' @param input_data Tibble containing the prepared long format price data with
#'   LCOE for the power sector
#' @param average_npm_power net profit margin in the power sector
#' @param start_year first year of the scenario
#'
#' @export
prepare_lcoe_adjusted_price_data_weo <- function(input_data,
                                                 average_npm_power,
                                                 start_year) {
  unadjusted_price_data <- input_data %>%
    dplyr::filter(.data$year >= start_year)

  prices_with_lcoe <- unadjusted_price_data %>%
    dplyr::filter(.data$sector == "Power", .data$indicator == "LCOE")

  implied_price <- prices_with_lcoe %>%
    dplyr::filter(.data$year == .env$start_year) %>%
    dplyr::mutate(implied_price = .data$price / (1 - .env$average_npm_power)) %>%
    dplyr::mutate(absolute_npm = .data$implied_price - .data$price) %>%
    dplyr::select(
      c(
        .data$source, .data$scenario, .data$scenario_geography, .data$sector,
        .data$technology, .data$unit, .data$implied_price, .data$absolute_npm
      )
    )

  prices_with_lcoe <- prices_with_lcoe %>%
    dplyr::group_by(
      .data$source, .data$scenario, .data$scenario_geography, .data$sector,
      .data$technology, .data$indicator, .data$unit
    ) %>%
    dplyr::arrange(
      .data$source, .data$scenario, .data$scenario_geography, .data$sector,
      .data$technology, .data$indicator, .data$unit, .data$year
    ) %>%
    dplyr::mutate(cost_factor = dplyr::first(.data$price) / .data$price) %>%
    dplyr::ungroup()

  implied_price_over_time <- prices_with_lcoe %>%
    dplyr::inner_join(
      implied_price,
      by = c("source", "scenario", "scenario_geography", "sector", "technology", "unit")
    )

  prices_adjusted <- implied_price_over_time %>%
    dplyr::mutate(
      price = .data$implied_price * .data$cost_factor,
      indicator = "price"
    ) %>%
    dplyr::select(colnames(input_data))

  prices_other_sectors <- unadjusted_price_data %>%
    dplyr::filter(!(.data$sector == "Power" & .data$indicator == "LCOE"))

  prices_adjusted <- prices_adjusted %>%
    dplyr::bind_rows(prices_other_sectors)

  return(prices_adjusted)
}

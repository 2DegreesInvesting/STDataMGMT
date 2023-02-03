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

#' This function reads in the price data from Oxford and calculates 
#' the LCOE for the power sector. 
#'
#' @param input_data_lcoe_oxford Dataet containg Oxford prices  
#' @param average_npm_power net profit margin in the power sector
#'
#' @export
prepare_lcoe_adjusted_price_data_oxford2021 <- function(input_data_lcoe_oxford,
                                                        average_npm_power) {
  start_year <- 2021
  
  data <- input_data_lcoe_oxford %>%
    dplyr::filter(.data$Sector == "Power") %>%
    dplyr::filter((stringr::str_detect(.data$Scenario, "Oxford"))) %>%
    dplyr::filter(.data$Year >= start_year) %>%
    dplyr::mutate(
      technology = dplyr::case_when(
        .data$Technology == "Natural gas" ~ "GasCap",
        .data$Technology == "Coal" ~ "CoalCap",
        .data$Sub_Technology == "Hydro" ~ "HydroCap",
        .data$Technology == "Nuclear"  ~ "NuclearCap",
        .data$Technology == "Renewables" & .data$Sub_Technology != "HydroCap" ~ "RenewablesCap",
        TRUE ~ .data$Technology),
      unit = "$/MWh",
      scenario = dplyr::case_when(
        .data$Scenario == "Oxford - fast_transition" ~ "fast_transition_oxford",
        .data$Scenario == "Oxford - no_transition" ~ "no_transition_oxford",
        .data$Scenario == "Oxford - slow_transition" ~ "slow_transition_oxford",
        TRUE ~ .data$Scenario
      )) %>%
    dplyr::filter(.data$scenario != "slow_transition_oxford") %>% 
    dplyr::select(-c(.data$Sub_Technology, .data$Technology, .data$Scenario)) %>%
    dplyr::rename(scenario_geography = .data$Region,
                  sector = .data$Sector,
                  year = .data$Year,
                  price = .data$LCOE)
  
  oil_cap <- data %>% dplyr::filter(.data$technology == "GasCap") %>%
    dplyr::mutate(technology = dplyr::if_else(.data$technology == "GasCap", "OilCap", .data$technology))
  
  renewables_cap <- data %>% dplyr::filter(.data$technology == "RenewablesCap") %>%
    dplyr::group_by(.data$scenario, .data$scenario_geography, .data$sector, .data$year, .data$technology) %>%
    dplyr::mutate(price = mean(.data$price)) %>%
    unique()
  
  data <- data %>% dplyr::filter(!.data$technology %in% c('RenewablesCap'))
  
  data <- dplyr::full_join(data, renewables_cap)
  data <- dplyr::full_join(data, oil_cap)
  

  implied_price <- data %>%
    dplyr::filter(.data$year == .env$start_year) %>%
    dplyr::mutate(implied_price = .data$price / (1 - .env$average_npm_power)) %>%
    dplyr::mutate(absolute_npm = .data$implied_price - .data$price) %>%
    dplyr::select(
      c(.data$scenario, .data$scenario_geography, .data$sector,
        .data$technology, .data$unit, .data$implied_price, .data$absolute_npm)
    )
 
  data <- data %>%
    dplyr::group_by(
      .data$scenario, .data$scenario_geography, .data$sector,
      .data$technology, .data$unit
    ) %>%
    dplyr::arrange(
      .data$scenario, .data$scenario_geography, .data$sector,
      .data$technology, .data$unit, .data$year
    ) %>%
    dplyr::mutate(cost_factor = dplyr::first(.data$price) / .data$price) %>%
    dplyr::ungroup()
  
  implied_price_over_time <- data %>%
    dplyr::inner_join(
      implied_price,
      by = c("scenario", "scenario_geography", "sector", "technology", "unit")
    )

  prices_adjusted <- implied_price_over_time %>%
    dplyr::mutate(
      price = .data$implied_price * .data$cost_factor,
      indicator = "price"
    ) %>%
    dplyr::select(-c(.data$cost_factor, .data$implied_price, .data$absolute_npm))
  
  #NOTE: we use Oxford LCOE data but match and label them as NGFS data 
  #In detail: NZ2050 -fast DN0 - fast B2DS - fast DT - fast NDC - low CP -low   

  prices_adjusted <- prices_adjusted %>% dplyr::mutate(GCAM = "GCAM", REMIND = "REMIND", MESSAGE = "MESSAGE") %>% 
    tidyr::pivot_longer(.data$GCAM:.data$MESSAGE, names_to = "model") %>% dplyr::select(-c(.data$value))

  oxford_fast_transition <- prices_adjusted %>% dplyr::filter(.data$scenario == "fast_transition_oxford")%>% 
  dplyr::select(-c(.data$scenario))%>%
  dplyr::mutate(NZ2050 ="NZ2050",DN0 = "DNO",B2DS = "B2DS",DT  = "DT") %>%
  tidyr::pivot_longer(.data$NZ2050:.data$DT, names_to = "scenario") %>% dplyr::select(-c(.data$value)) 

  oxford_slow_transition <- prices_adjusted %>% dplyr::filter(.data$scenario == "no_transition_oxford") %>%
    dplyr::select(-c(.data$scenario)) %>%
    dplyr::mutate(NDC ="NDC",CP = "CP") %>%
    tidyr::pivot_longer(.data$NDC:.data$CP, names_to = "scenario") %>% dplyr::select(-c(.data$value))

  prices_adjusted_final <- dplyr::full_join(oxford_fast_transition, oxford_slow_transition) %>%
    tidyr::unite("scenario", c(.data$model, .data$scenario), sep = "_")


  return(prices_adjusted_final)
  
}
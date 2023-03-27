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
        .data$Technology == "Nuclear" ~ "NuclearCap",
        .data$Technology == "Renewables" & .data$Sub_Technology != "HydroCap" ~ "RenewablesCap",
        TRUE ~ .data$Technology
      ),
      # NOTE: rename World to Global to fit the ST scenario geography names
      scenario_geography = dplyr::case_when(
        .data$Region == "World" ~ "Global",
        TRUE ~ .data$Region
      ),
      unit = "$/MWh",
      scenario = dplyr::case_when(
        .data$Scenario == "Oxford - fast_transition" ~ "fast_transition_oxford",
        .data$Scenario == "Oxford - no_transition" ~ "no_transition_oxford",
        .data$Scenario == "Oxford - slow_transition" ~ "slow_transition_oxford",
        TRUE ~ .data$Scenario
      )
    ) %>%
    dplyr::filter(.data$scenario != "slow_transition_oxford") %>%
    dplyr::select(-c(.data$Sub_Technology, .data$Technology, .data$Scenario, .data$Region)) %>%
    dplyr::rename(
      sector = .data$Sector,
      year = .data$Year,
      price = .data$LCOE
    )

  oil_cap <- data %>%
    dplyr::filter(.data$technology == "GasCap") %>%
    dplyr::mutate(technology = dplyr::if_else(.data$technology == "GasCap", "OilCap", .data$technology))

  renewables_cap <- data %>%
    dplyr::filter(.data$technology == "RenewablesCap") %>%
    dplyr::group_by(.data$scenario, .data$scenario_geography, .data$sector, .data$year, .data$technology) %>%
    dplyr::mutate(price = mean(.data$price)) %>%
    unique()

  data <- data %>% dplyr::filter(!.data$technology %in% c("RenewablesCap"))

  data <- dplyr::full_join(data, renewables_cap)
  data <- dplyr::full_join(data, oil_cap)


  implied_price <- data %>%
    dplyr::filter(.data$year == .env$start_year) %>%
    dplyr::mutate(implied_price = .data$price / (1 - .env$average_npm_power)) %>%
    dplyr::mutate(absolute_npm = .data$implied_price - .data$price) %>%
    dplyr::select(
      c(
        .data$scenario, .data$scenario_geography, .data$sector,
        .data$technology, .data$unit, .data$implied_price, .data$absolute_npm
      )
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

  # Function to add the years from 2070 to 2100. NAs will be linearly extrapolated at the step below

  add_years <- function(data, start, end) {
    technologies <- c("OilCap", "CoalCap", "GasCap", "RenewablesCap", "HydroCap", "NuclearCap")
    scenarios <- c("fast_transition_oxford", "no_transition_oxford", "slow_transition_oxford")
    sector <- unique(data$sector)
    scenario_geography <- unique(data$scenario_geography)
    unit <- unique(data$unit)
    indicator <- unique(data$indicator)
    new_data <- data
    for (year in start:end) {
      for (technology in technologies) {
        for (scenario in scenarios) {
          new_row <- data.frame(sector = sector, scenario_geography = scenario_geography, year = year, price = NA, technology = technology, unit = unit, scenario = scenario, indicator = indicator, stringsAsFactors = FALSE)
          new_data <- rbind(new_data, new_row)
        }
      }
    }
    return(new_data)
  }

  tech <- unique(prices_adjusted$technology)
  scen <- unique(prices_adjusted$scenario)

  data <- add_years(prices_adjusted, 2070, 2100)

  ## Linear extrapolation using the last 20 years of observation

  for (i in tech) {
    for (j in scen) {
      model <- stats::lm(price ~ year, data = data[data$year >= 2049 & data$year <= 2069 & data$technology == i & data$scenario == j, ])
      data$price[data$technology == i & data$scenario == j] <- ifelse(is.na(data$price[data$technology == i & data$scenario == j]), model$coefficients[2] * data$year[data$technology == i & data$scenario == j] + model$coefficients[1], data$price[data$technology == i & data$scenario == j])
    }
  }
  prices_adjusted <- data

  ## We use Oxford LCOEs for NGFS and Oxford scenarios. Here we rename the Oxford scenarios and then in a second step we match Oxford data with NGFS scenarios
  ## Both datasets are then merged together.

  ## Oxford scenarios
  prices_oxford <- prices_adjusted %>%
    dplyr::mutate(
        scenario = dplyr::if_else(.data$scenario == "fast_transition_oxford", "Oxford2021_fast", .data$scenario),
        scenario = dplyr::if_else(.data$scenario == "no_transition_oxford", "Oxford2021_base", .data$scenario)) %>%
    dplyr::filter(scenario != "slow_transition_oxford")

  ## For NGFS scenarios
  # NOTE: we use Oxford LCOE data but match and label them as NGFS data
  # In detail: NZ2050 -fast DN0 - fast B2DS - fast DT - fast NDC - low CP -low

  prices_adjusted <- prices_adjusted %>%
    dplyr::mutate(GCAM = "GCAM", REMIND = "REMIND", MESSAGE = "MESSAGE") %>%
    tidyr::pivot_longer(.data$GCAM:.data$MESSAGE, names_to = "model") %>%
    dplyr::select(-c(.data$value))

  oxford_fast_transition <- prices_adjusted %>%
    dplyr::filter(.data$scenario == "fast_transition_oxford") %>%
    dplyr::select(-c(.data$scenario)) %>%
    dplyr::mutate(NZ2050 = "NZ2050", DN0 = "DNO", B2DS = "B2DS", DT = "DT") %>%
    tidyr::pivot_longer(.data$NZ2050:.data$DT, names_to = "scenario") %>%
    dplyr::select(-c(.data$value))

  oxford_slow_transition <- prices_adjusted %>%
    dplyr::filter(.data$scenario == "no_transition_oxford") %>%
    dplyr::select(-c(.data$scenario)) %>%
    dplyr::mutate(NDC = "NDC", CP = "CP") %>%
    tidyr::pivot_longer(.data$NDC:.data$CP, names_to = "scenario") %>%
    dplyr::select(-c(.data$value))

  prices_adjusted_final <- dplyr::full_join(oxford_fast_transition, oxford_slow_transition) %>%
    tidyr::unite("scenario", c(.data$model, .data$scenario), sep = "_") %>%
    dplyr::mutate(scenario = paste("NGFS2021", .data$scenario, sep = "_"))

  # merging NGFS and Oxford prices
  prices_adjusted_final <- dplyr::full_join(prices_adjusted_final, prices_oxford)

  return(prices_adjusted_final)
}

### IPR Function LCOE adjustment
### This function reads the wrangled LCOE data generated by prepare_price_data_long_Power_IPR2021 and adjusts the LCOE to generate Power prices.
### This function then matches the WEO2021 scenarios to the IPR scenarios:
### In the current approach, we match IEA SDS to both IPR RPS and IPR FPS


prepare_lcoe_adjusted_price_data_IPR2021 <- function(input_data,
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


  ## IPR adjustment

  ## Only global region
  prices_adjusted <- prices_adjusted[prices_adjusted$scenario_geography == "Global", ]

  ## Matching IEA scenarios to IPR scenarios
  FPS <- prices_adjusted[prices_adjusted$scenario == "SDS", ]

  FPS$scenario[FPS$scenario == "SDS"] <- "IPR2021_FPS"

  RPS <- prices_adjusted[prices_adjusted$scenario == "SDS", ]

  RPS$scenario[RPS$scenario == "SDS"] <- "IPR2021_RPS"

  prices_adjusted <- rbind(FPS, RPS)

  return(prices_adjusted)
}

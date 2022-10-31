devtools::load_all()

# set overall parameters----
# based on internal research, see stress.testing.internal repo
average_npm_power <- 0.115

# prepare price data WEO 2019----

## read input data----
input_path_fossil_fuels <- path_dropbox_2dii(
  "PortCheck",
  "00_Data",
  "01_ProcessedData",
  "03_ScenarioData",
  "RawData",
  "WEO2019_fossil_fuel_prices_by_scenario.csv"
)

input_data_fossil_fuels <- readr::read_csv(
  file.path(input_path_fossil_fuels),
  col_types = readr::cols(
    source = "c",
    sector = "c",
    unit = "c",
    scenario_geography = "c",
    scenario = "c",
    .default = readr::col_number()
  )
)

input_path_power <- path_dropbox_2dii(
  "PortCheck",
  "00_Data",
  "01_ProcessedData",
  "03_ScenarioData",
  "RawData",
  "WEO2019_power_generation_technology_costs.csv"
)

input_data_power <- readr::read_csv(
  file.path(input_path_power),
  col_types = readr::cols(
    source = "c",
    scenario = "c",
    region = "c",
    technology = "c",
    indicator = "c",
    unit = "c",
    .default = readr::col_number()
  )
)

### set parameters WEO2019----
start_year <- 2019

## prepare data----
price_data_long_WEO2019 <- prepare_price_data_long_WEO2019(
  input_data_fossil_fuel = input_data_fossil_fuels,
  input_data_power = input_data_power
)

price_data_long_adjusted_WEO2019 <- prepare_lcoe_adjusted_price_data_weo(
  input_data = price_data_long_WEO2019,
  average_npm_power = average_npm_power,
  start_year = start_year
) %>%
  dplyr::mutate(scenario = paste("WEO2019", .data$scenario, sep = "_")) %>%
  dplyr::select(-.data$source)

# prepare price data WEO 2020----

## read input data----
input_path_fossil_fuels <- path_dropbox_2dii(
  "PortCheck",
  "00_Data",
  "01_ProcessedData",
  "03_ScenarioData",
  "RawData",
  "WEO2020_fossil_fuel_prices_by_scenario.csv"
)

input_data_fossil_fuels <- readr::read_csv(
  file.path(input_path_fossil_fuels),
  col_types = readr::cols(
    source = "c",
    sector = "c",
    unit = "c",
    scenario_geography = "c",
    scenario = "c",
    .default = readr::col_number()
  )
)

input_path_power <- path_dropbox_2dii(
  "PortCheck",
  "00_Data",
  "01_ProcessedData",
  "03_ScenarioData",
  "RawData",
  "WEO2020_power_generation_technology_costs.csv"
)

input_data_power <- readr::read_csv(
  file.path(input_path_power),
  col_types = readr::cols(
    source = "c",
    scenario = "c",
    region = "c",
    technology = "c",
    indicator = "c",
    unit = "c",
    .default = readr::col_number()
  )
)

### set parameters WEO2020----
start_year <- 2020

# prepare data
price_data_long_WEO2020 <- prepare_price_data_long_WEO2020(
  input_data_fossil_fuel = input_data_fossil_fuels,
  input_data_power = input_data_power
)

price_data_long_adjusted_WEO2020 <- prepare_lcoe_adjusted_price_data_weo(
  input_data = price_data_long_WEO2020,
  average_npm_power = average_npm_power,
  start_year = start_year
) %>%
  dplyr::mutate(scenario = paste("WEO2020", .data$scenario, sep = "_")) %>%
  dplyr::select(-.data$source)


# prepare price data WEO 2021----

# raw data obtained from WEO2021 report, page 101
## read input data----
input_path_fossil_fuels <- path_dropbox_2dii(
  "PortCheck",
  "00_Data",
  "01_ProcessedData",
  "03_ScenarioData",
  "RawData",
  "WEO2021_fossil_fuel_prices_by_scenario.csv"
)

input_data_fossil_fuels <- readr::read_csv(
  file.path(input_path_fossil_fuels),
  col_types = readr::cols(
    source = "c",
    sector = "c",
    unit = "c",
    scenario_geography = "c",
    scenario = "c",
    .default = readr::col_number()
  )
)

# raw data obtained from WEO2021 report, pages 333-336
input_path_power <- path_dropbox_2dii(
  "PortCheck",
  "00_Data",
  "01_ProcessedData",
  "03_ScenarioData",
  "RawData",
  "WEO2021_power_generation_technology_costs.csv"
)

input_data_power <- readr::read_csv(
  file.path(input_path_power),
  col_types = readr::cols(
    source = "c",
    scenario = "c",
    region = "c",
    technology = "c",
    indicator = "c",
    unit = "c",
    .default = readr::col_number()
  )
)

### set parameters WEO2021----
start_year <- 2021

# prepare data
price_data_long_WEO2021 <- prepare_price_data_long_WEO2021(
  input_data_fossil_fuel = input_data_fossil_fuels,
  input_data_power = input_data_power
)

price_data_long_adjusted_WEO2021 <- prepare_lcoe_adjusted_price_data_weo(
  input_data = price_data_long_WEO2021,
  average_npm_power = average_npm_power,
  start_year = start_year
) %>%
  dplyr::mutate(scenario = paste("WEO2021", .data$scenario, sep = "_")) %>%
  dplyr::select(-.data$source)

# ### set parameters GECO2019----

# No GECO 2019 price data to prepare at this point.
# We only use Automotive sector and that currently does not have price or cost data

## combine and write all price data----

price_data_long_adjusted <- price_data_long_adjusted_WEO2019 %>%
  dplyr::bind_rows(price_data_long_adjusted_WEO2020) %>%
  dplyr::bind_rows(price_data_long_adjusted_WEO2021)

price_data_long_adjusted %>%
  readr::write_csv(file.path("data-raw", "price_data_long.csv"))

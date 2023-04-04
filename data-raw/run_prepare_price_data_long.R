devtools::load_all()

# set overall parameters----
# based on internal research, see stress.testing.internal repo
average_npm_power <- 0.115

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



# prepare price data NGFS 2021

## read input data
input_path_fossil_fuels_ngfs <- file.path("data-raw", "raw_price_data_long_NGFS2021.csv")
input_data_fossil_fuels_ngfs <- readr::read_csv(
  input_path_fossil_fuels_ngfs,
  col_types = readr::cols(
    Model = "c",
    Scenario = "c",
    Region = "c",
    Variable = "c",
    category_a = "c",
    category_b = "c",
    category_c = "c",
    Unit = "c",
    year = "d",
    value = "d",
    .default = readr::col_number()
  )
)

## read LCOE price data Oxford2021
input_path_lcoe_oxford <- file.path("data-raw", "raw_Oxford_LCOE_wrangled.csv")

input_data_lcoe_oxford <- readr::read_delim(
  file.path(input_path_lcoe_oxford),
  col_types = readr::cols(
    Scenario = "c",
    Sector = "c",
    Region = "c",
    Technology = "c",
    Sub_Technology = "c",
    Year = "d",
    LCOE = "d",
    .default = readr::col_number()
  )
)

price_data_long_NGFS2021 <- prepare_price_data_long_NGFS2021(
  input_data_fossil_fuels_ngfs = input_data_fossil_fuels_ngfs
)

lcoe_adjusted_price_data_oxford2021 <- prepare_lcoe_adjusted_price_data_oxford2021(
  input_data_lcoe_oxford = input_data_lcoe_oxford,
  average_npm_power = average_npm_power
)

price_data_long_adjusted_NGFS2021 <- price_data_long_NGFS2021 %>%
  dplyr::bind_rows(lcoe_adjusted_price_data_oxford2021)


### prepare price data IPR 2021

## prepare IPR 2021 Fossil Fuel price data
input_path_fossil_fuels_ipr <- file.path("data-raw", "raw_price_data_long_IPR2021.csv")

input_data_fossil_fuels_ipr <- readr::read_delim(
  file.path(input_path_fossil_fuels_ipr),
  col_types = readr::cols(
    Scenario = "c",
    Region = "c",
    Variable_class = "c",
    Sub_variable_class_1 = "c",
    Units = "c",
    year = "d",
    value = "d",
    .default = readr::col_number()
  )
)

price_data_long_IPR2021 <- prepare_price_data_long_IPR2021(input_data_fossil_fuels_ipr)

## prepare IPR 2021 Power price data
## IPR prices for the power sector uses LCOE data from WEO2021 (input_data_power, see above)


price_data_power_IPR2021 <- prepare_price_data_long_Power_IPR2021(input_data_power)

lcoe_adjusted_price_data_IPR2021 <- prepare_lcoe_adjusted_price_data_IPR2021(
  input_data = price_data_power_IPR2021,
  average_npm_power = average_npm_power,
  start_year = start_year
) %>%
  dplyr::select(-.data$source)


### Total combined IPR2021 price data

price_data_long_adjusted_IPR2021 <- price_data_long_IPR2021 %>%
  dplyr::bind_rows(lcoe_adjusted_price_data_IPR2021)

## Oxford Price data

## Fossil Fuel prices
input_path_fossil_fuels_oxf <- file.path("data-raw", "raw_price_data_long_OXF2021.csv")

input_data_fossil_fuels_oxf <- readr::read_delim(
  file.path(input_path_fossil_fuels_oxf),
  col_types = readr::cols(
    Scenario = "c",
    Sector = "c",
    Region = "c",
    Technology = "c",
    Year = "d",
    LCOE = "d",
    .default = readr::col_number()
  )
)

price_data_long_adjusted_OXF2021 <- prepare_price_data_long_Oxf2021(input_data_fossil_fuels_oxf)

### Oxford Power prices
### Oxford power prices are already in the data through lcoe_adjusted_price_data_oxford2021

## combine and write all price data----

price_data_long_adjusted <- price_data_long_adjusted_WEO2021 %>%
  dplyr::bind_rows(price_data_long_adjusted_NGFS2021) %>%
  dplyr::bind_rows(price_data_long_adjusted_IPR2021) %>%
  dplyr::bind_rows(price_data_long_adjusted_OXF2021)


price_data_long_adjusted %>%
  readr::write_csv(file.path("data-raw", "price_data_long.csv"))

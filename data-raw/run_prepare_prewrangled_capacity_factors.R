devtools::load_all()

# prepare capacity factor data WEO 2019

## read data
input_path <- r2dii.utils::path_dropbox_2dii(
  "PortCheck", "00_Data",
  "01_ProcessedData", "03_ScenarioData", "RawData", "WEO2019_Raw_Data.csv"
)

data <- readr::read_csv(
  input_path,
  col_types = readr::cols(
    Source = "c",
    Indicator = "c",
    Sector = "c",
    Units = "c",
    Scenario = "c",
    ScenarioGeography = "c",
    Technology = "c",
    Sub_Technology = "c",
    SourceSheet = "c",
    .default = readr::col_number()
  )
)

## prepare data
prepared_data_WEO2019 <- prepare_prewrangled_capacity_factors_WEO2019(data)


# prepare capacity factor data WEO 2020

## read data
input_path <- r2dii.utils::path_dropbox_2dii(
  "PortCheck", "00_Data",
  "01_ProcessedData", "03_ScenarioData", "RawData", "WEO2020_Raw_Data.csv"
)

data <- readr::read_csv(
  input_path,
  col_types = readr::cols(
    Source = "c",
    Indicator = "c",
    Sector = "c",
    Units = "c",
    Scenario = "c",
    ScenarioGeography = "c",
    Technology = "c",
    Sub_Technology = "c",
    SourceSheet = "c",
    .default = readr::col_number()
  )
)

## prepare data
prepared_data_WEO2020 <- prepare_prewrangled_capacity_factors_WEO2020(data)

# prepare capacity factor data WEO 2021

# NOTE: the WEO2021 raw data did not deliver the required data points to
# sufficiently cover all scenario geographies. We therefor use the capacity
# factors from WEO2020 again
# For consistency in the code, we relabel the source to WEO2021 and we use
# capacity factors from SDS to fill in for the NZE scenario. We also use STEPS
# capacity factors to fill in for the APS scenario as - in both cases - these
# seem to be the closest in narrative.

## read data
input_path <- r2dii.utils::path_dropbox_2dii(
  "PortCheck", "00_Data",
  "01_ProcessedData", "03_ScenarioData", "RawData", "WEO2020_Raw_Data.csv"
)

data <- readr::read_csv(
  input_path,
  col_types = readr::cols(
    Source = "c",
    Indicator = "c",
    Sector = "c",
    Units = "c",
    Scenario = "c",
    ScenarioGeography = "c",
    Technology = "c",
    Sub_Technology = "c",
    SourceSheet = "c",
    .default = readr::col_number()
  )
)

## prepare data
prepared_data_WEO2021 <- prepare_prewrangled_capacity_factors_WEO2021(data)


## combine and write data
prepared_data <- prepared_data_WEO2019 %>%
  dplyr::bind_rows(prepared_data_WEO2020) %>%
  dplyr::bind_rows(prepared_data_WEO2021)

prepared_data %>% readr::write_csv(
  file.path("data-raw", "prewrangled_capacity_factors.csv")
)

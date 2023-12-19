devtools::load_all()

##set start year 
# start_year <- 2022 # defined in workflow.R

# prepare capacity factor data WEO 2021

# NOTE: the WEO2021 raw data did not deliver the required data points to
# sufficiently cover all scenario geographies. We therefor use the capacity
# factors from WEO2020 again
# For consistency in the code, we relabel the source to WEO2021 and we use
# capacity factors from SDS to fill in for the NZE scenario. We also use STEPS
# capacity factors to fill in for the APS scenario as - in both cases - these
# seem to be the closest in narrative.

## read data
input_path <- fs::path(
  "data-raw",
  "capacity_factors_data",
  "WEO2020_Raw_Data.csv"
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
prepared_data_WEO2021 <- prepare_prewrangled_capacity_factors_WEO2021(data, start_year = start_year)

## read data
input_path <- fs::path("data-raw", "capacity_factors_data","raw_capacity_factors_NGFS2021.csv")

data <- readr::read_csv(
  input_path,
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

## prepare data
prepared_data_NGFS2021 <- prepare_capacity_factors_NGFS2021(data, start_year = start_year)

### IPR data

input_path <- fs::path("data-raw", "capacity_factors_data","raw_capacity_factors_IPR2021.csv")

data <- readr::read_csv(
  input_path,
  col_types = readr::cols(
    Variable_class = "c",
    Sub_variable_class_1 = "c",
    Sub_variable_class_2 = "c",
    Scenario = "c",
    Region = "c",
    Sector = "c",
    Units = "c",
    year = "d",
    value = "d",
    .default = readr::col_number()
  )
)

## prepare IPR data
prepared_data_IPR2021 <- prepare_capacity_factors_IPR2021(data, start_year = start_year)

## IPR baseline CF is a duplicate of IPR2021_FPS
IPR_baseline <- prepare_capacity_factors_IPR2021_baseline(prepared_data_IPR2021)

# merging IPR CF data
prepared_data_IPR2021 <- dplyr::full_join(prepared_data_IPR2021, IPR_baseline)

### Oxford data
# Oxford uses Capacity Factors from WEO2021

prepared_data_OXF2021 <- prepare_capacity_factors_OXF2021(prepared_data_WEO2021)

## combine and write data
prepared_data <- prepared_data_WEO2021 %>%
  dplyr::bind_rows(prepared_data_NGFS2021) %>%
  dplyr::bind_rows(prepared_data_IPR2021) %>%
  dplyr::bind_rows(prepared_data_OXF2021)

prepared_data %>% 
  dplyr::rename(ald_business_unit=.data$technology) %>%
  readr::write_csv(
  file.path("data-raw", "st_inputs","prewrangled_capacity_factors.csv")
)

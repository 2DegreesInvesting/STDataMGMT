devtools::load_all()

# make sure to set the relevant start year when running the data preparation
# example:
start_year_ngfs <- 2022

input_path <- file.path("data-raw", "raw_ngfs_carbon_price.csv")
data <- readr::read_csv(
  input_path,
  col_types = readr::cols(
    Model = "c",
    Scenario = "c",
    Region = "c",
    Variable = "c",
    Unit = "c",
    .default = readr::col_number()
  )
)
# NOTE: the carbon prices in the raw input file were manually set to 0 prior to
# 2025 in 2 cases and prior to 2030 in the case of the hot house world to
# facilitate processing and per request of the research.
ngfs_carbon_price <- data %>%
  prepare_ngfs_carbon_price(start_year = start_year_ngfs)

ngfs_carbon_price %>%
  readr::write_csv(file.path("data-raw", "ngfs_carbon_price.csv"))

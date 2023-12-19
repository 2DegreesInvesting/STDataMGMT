# This script generates the abcd input from asset_resolution data
# as well as the production_type.rda reference dataset in this packge
# TODO all the code before abcd_data should be translated to SQL

devtools::load_all()

data(scenarios_geographies)

## PARAMETERS

output_path_stress_test_input <-
  fs::path(
    "data-raw",
    "st_inputs",
  "abcd_stress_test_input",
  ext = "csv"
)
  

# start_year <- 2022 # defined in workflow.R
time_horizon <- 5
additional_year <- NULL
sector_list <- c("Automotive", "Power", "Oil&Gas", "Coal")
km_per_vehicle <- 15000

DB_company_activities  <-  arrow::read_parquet(fs::path("data-raw","DBs","DB_company_activities", ext="parquet"))
DB_company_emissions  <-  arrow::read_parquet(fs::path("data-raw","DBs","DB_company_emissions", ext="parquet"))

abcd_stress_test_input <-
  prepare_abcd_data(
    company_activities = DB_company_activities,
    company_emissions = DB_company_emissions,
    scenarios_geographies = scenarios_geographies, # loaded from package
    start_year = start_year,
    time_horizon = time_horizon,
    additional_year = additional_year,
    km_per_vehicle = km_per_vehicle,
    sector_list = sector_list
  )

abcd_stress_test_input %>% 
  assertr::verify(all(colSums(is.na(.)) == 0))

abcd_stress_test_input %>% readr::write_csv(output_path_stress_test_input)

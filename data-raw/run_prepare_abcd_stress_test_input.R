# This script generates the abcd input from asset_resolution data
# as well as the production_type.rda reference dataset in this packge
# TODO all the code before abcd_data should be translated to SQL

devtools::load_all()

data(scenarios_geographies)

## PARAMETERS
path_ar_data_raw <-
  r2dii.utils::path_dropbox_2dii(
    "ST_INPUTS",
    "ST_INPUTS_PRODUCTION"
  )

output_path_stress_test_input <-
  r2dii.utils::path_dropbox_2dii(
    "ST_INPUTS",
    "ST_INPUTS_MASTER"
  )

start_year <- 2022
time_horizon <- 5
additional_year <- NULL
sector_list <- c("Automotive", "Power", "Oil&Gas", "Coal")
km_per_vehicle <- 15000


outputs_list <- prepare_asset_impact_data(ar_data_path=ar_data_path)
company_activities <- outputs_list[["company_activities"]]
company_emissions <- outputs_list[["company_emissions"]]

# THE PART ABOVE IS ASSET RESOLUTION SPECIFIC
# ========================
# CONVERT CLEAN COMPANY DATA TO ABCD BELOW

abcd_stress_test_input <-
  prepare_abcd_data(
    company_activities = company_activities,
    company_emissions = company_emissions,
    scenarios_geographies = scenarios_geographies, # loaded from package
    start_year = start_year,
    time_horizon = time_horizon,
    additional_year = additional_year,
    km_per_vehicle = km_per_vehicle,
    sector_list = sector_list
  )

abcd_stress_test_input %>% 
  assertr::verify(all(colSums(is.na(.)) == 0))

abcd_stress_test_input %>% readr::write_csv(fs::path(
  output_path_stress_test_input,
  "abcd_stress_test_input",
  ext = "csv"
))

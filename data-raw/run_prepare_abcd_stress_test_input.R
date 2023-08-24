devtools::load_all()

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

bench_regions <-
  readr::read_rds(here::here("data-raw", "bench_regions.rds"))

company_activities <-
  read_asset_resolution(
    fs::path(path_ar_data_raw,
      "AR-Company-Indicators_2022Q4",
      ext = "xlsx"
    ),
    sheet_name = "Company Activities"
  )
company_emissions <-
  read_asset_resolution(
    fs::path(path_ar_data_raw,
      "AR-Company-Indicators_2022Q4",
      ext = "xlsx"
    ),
    sheet_name = "Company Emissions"
  )

outputs_list <-
  prepare_assets_data(company_activities, company_emissions)

clean_company_activities <- outputs_list[["company_activities"]]
clean_company_emissions <- outputs_list[["company_emissions"]]

abcd_data <-
  prepare_abcd_data(
    company_activities = clean_company_activities,
    company_emissions = clean_company_emissions,
    scenarios_geographies = bench_regions,
    start_year = start_year,
    time_horizon = time_horizon,
    additional_year = additional_year,
    km_per_vehicle = km_per_vehicle,
    sector_list = sector_list
  )

abcd_data %>%
  assertr::verify(all(colSums(is.na(.)) == 0))

abcd_data %>% readr::write_csv(fs::path(
  output_path_stress_test_input,
  "abcd_stress_test_input",
  ext = "csv"
))

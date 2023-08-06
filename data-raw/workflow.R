devtools::load_all()

## PARAMETERS
#TODO config file

#### bench_regions renaming
matching_tol <- 1

bench_regions_input_path <-
  bench_regions_output_path <-
  here::here("data-raw", "bench_regions.csv")

path_prewrangled_capacity_factors <-
  here::here("data-raw", "prewrangled_capacity_factors.csv")
path_price_data_long <-
  here::here("data-raw", "price_data_long.csv")
path_Scenarios_AnalysisInput <-
  here::here("data-raw", "Scenarios_AnalysisInput_2021.csv")

#### abcd_stress_test_input
path_ar_data_raw <-
  r2dii.utils::path_dropbox_2dii(
    "ST_INPUTS",
    "ST_INPUTS_PRODUCTION",
    "2023-02-15_AI_2DII Germany-Company-Indicators_2022Q4.xlsx"
  )

output_path_stress_test_inputs <-
  r2dii.utils::path_dropbox_2dii("ST_INPUTS",
                                 "ST_INPUTS_MASTER",
                                 "abcd_stress_test_input.csv")

start_year <- 2021
time_horizon <- 5
additional_year <- NULL
sector_list <- c("Automotive", "Power", "Oil&Gas", "Coal")


## MAIN

#### rename bench_regions
bench_regions <-
  readr::read_csv(bench_regions_input_path, na = c(""))

output_list <- regroup_and_rename_geographies(
  bench_regions = bench_regions,
  path_prewrangled_capacity_factors = path_prewrangled_capacity_factors,
  path_price_data_long = path_price_data_long,
  path_Scenarios_AnalysisInput = path_Scenarios_AnalysisInput,
  matching_tol = matching_tol
)

trisk_input_dfs_renamed <- output_list[["trisk_input_dfs"]]
bench_regions_renamed <- output_list[["bench_regions"]]


### abcd_stress_test_input
company_activities <-
  read_asset_resolution(path_ar_data_raw, sheet_name = "Company Activities")
company_emissions <-
  read_asset_resolution(path_ar_data_raw, sheet_name = "Company Emissions")

output_list <-
  prepare_assets_data(company_activities, company_emissions)

clean_company_activities <- output_list[["company_activities"]]
clean_company_emissions <- output_list[["company_emissions"]]

abcd_data <-
  prepare_abcd_data(
    company_activities = clean_company_activities,
    company_emissions = clean_company_emissions,
    scenarios_geographies = bench_regions_renamed,
    start_year = 2017,
    time_horizon = 5,
    additional_year = NULL,
    sector_list = c("HDV", "Automotive", "Power", "Fossil Fuels", "Oil&Gas", "Coal")
  )


### WRITE RESULTS
bench_regions %>% readr::write_csv(bench_regions_output_path, na = c(""))

for (fp in names(trisk_input_dfs)) {
  readr::write_csv(trisk_input_dfs[[fp]], fp)
}

abcd_data %>% readr::write_csv(output_path_stress_test_inputs)

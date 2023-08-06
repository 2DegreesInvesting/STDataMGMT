devtools::load_all()

# PARAMETERS
matching_tol <- 1
bench_regions_input_path <-
  bench_regions_output_path <-
  here::here("data-raw", "bench_regions.csv")
bench_regions <-
  readr::read_csv(bench_regions_input_path, na = c(""))

path_prewrangled_capacity_factors <-
  here::here("data-raw", "prewrangled_capacity_factors.csv")
path_price_data_long <-
  here::here("data-raw", "price_data_long.csv")
path_Scenarios_AnalysisInput <-
  here::here("data-raw", "Scenarios_AnalysisInput_2021.csv")


## RENAMING
output_list <- regroup_and_rename_geographies(
  bench_regions = bench_regions,
  path_prewrangled_capacity_factors = path_prewrangled_capacity_factors,
  path_price_data_long = path_price_data_long,
  path_Scenarios_AnalysisInput = path_Scenarios_AnalysisInput,
  matching_tol = matching_tol
)

trisk_input_dfs <- output_list[["trisk_input_dfs"]]
bench_regions <- output_list[["bench_regions"]]

## ------------------------------ WRITE OUTPUTS
bench_regions %>% readr::write_csv(bench_regions_output_path, na = c(""))

for (fp in names(trisk_input_dfs)) {
  readr::write_csv(trisk_input_dfs[[fp]], fp)
}

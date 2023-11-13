
print("=================== RUNNING run_prepare_Scenarios_AnalysisInput_start_year ===================")
source(fs::path("data-raw", "run_prepare_Scenarios_AnalysisInput_start_year.R"))
rm(list = ls())

print("=================== RUNNING run_prepare_prewrangled_capacity_factors ===================")
source(fs::path("data-raw", "run_prepare_prewrangled_capacity_factors.R"))
rm(list = ls())

print("=================== RUNNING run_prepare_price_data_long ===================")
source(fs::path("data-raw", "run_prepare_price_data_long.R"))
rm(list = ls())

print("=================== RUNNING run_rename_geographies ===================")
source(fs::path("data-raw", "run_rename_geographies.R"))
rm(list = ls())

print("=================== RUNNING run_prepare_prewrangled_financial_data_stress_test ===================")
source(fs::path("data-raw", "run_prepare_prewrangled_financial_data_stress_test.R"))
rm(list = ls())

print("=================== RUNNING run_prepare_abcd_stress_test_input ===================")
source(fs::path("data-raw", "run_prepare_abcd_stress_test_input.R"))
rm(list = ls())
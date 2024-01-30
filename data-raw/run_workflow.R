start_year <- 2022

# OPEN SOURCE DATA

print("=================== RUNNING run_prepare_Scenarios_AnalysisInput ===================")
source(fs::path("data-raw", "run_prepare_Scenarios_AnalysisInput.R"))
rm(list = ls()[ls() != "start_year"])

print("=================== RUNNING run_prepare_prewrangled_capacity_factors ===================")
source(fs::path("data-raw", "run_prepare_prewrangled_capacity_factors.R"))
rm(list = ls()[ls() != "start_year"])

print("=================== RUNNING run_prepare_price_data_long ===================")
source(fs::path("data-raw", "run_prepare_price_data_long.R"))
rm(list = ls()[ls() != "start_year"])

print("=================== RUNNING run_prepare_ngfs_carbon_price ===================")
source(fs::path("data-raw", "run_prepare_ngfs_carbon_price.R"))
rm(list = ls()[ls() != "start_year"])

print("=================== RUNNING run_rename_geographies ===================")
source(fs::path("data-raw", "run_rename_geographies.R"))
rm(list = ls()[ls() != "start_year"])



# ===== SAVE AS PACKAGE DATA

Scenarios_AnalysisInput <- readr::read_csv(here::here("data-raw", "st_inputs", "Scenarios_AnalysisInput.csv"))
usethis::use_data(Scenarios_AnalysisInput, overwrite = TRUE)

prewrangled_capacity_factors <- readr::read_csv(here::here("data-raw", "st_inputs", "prewrangled_capacity_factors.csv"))
usethis::use_data(prewrangled_capacity_factors, overwrite = TRUE)

price_data_long <- readr::read_csv(here::here("data-raw", "st_inputs", "price_data_long.csv"))
usethis::use_data(price_data_long, overwrite = TRUE)

ngfs_carbon_price <- readr::read_csv(here::here("data-raw", "st_inputs", "ngfs_carbon_price.csv"))
usethis::use_data(ngfs_carbon_price, overwrite = TRUE)



# CLOSED SOURCE DATA


print("=================== RUNNING run_prepare_abcd_stress_test_input ===================")
source(fs::path("data-raw", "run_prepare_abcd_stress_test_input.R"))
rm(list = ls()[ls() != "start_year"])

print("=================== RUNNING run_prepare_prewrangled_financial_data_stress_test ===================")
source(fs::path("data-raw", "run_prepare_prewrangled_financial_data_stress_test.R"))
rm(list = ls()[ls() != "start_year"])

# ===== SAVE TO DROPBOX

for (fp in c("abcd_stress_test_input.csv","financial_data_stress_test.csv")){
  
  readr::write_csv(
    readr::read_csv(here::here("data-raw", "st_inputs", fp)),
    r2dii.utils::path_dropbox_2dii(fs::path("ST Inputs", "ST_INPUTS_MASTER", fp))
  )

}


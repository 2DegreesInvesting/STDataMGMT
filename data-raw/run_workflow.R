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


# CLOSED SOURCE DATA

# used only in run_prepare_abcd_stress_test_input.R 
# but kept in environment until end of script 
# countrycode::codelist %>%
#   filter(country.name.en == "Slobakia") %>%
#   dplyr::pull(.data$ecb)
country_filter <- c() 

# those 2 are deleted from the environment after the run_prepare_abcd_stress_test_input.R 
filter_hqs <- FALSE
filter_assets <- FALSE

print("=================== RUNNING run_prepare_abcd_stress_test_input ===================")
source(fs::path("data-raw", "run_prepare_abcd_stress_test_input.R"))
rm(list = ls()[ls() != c("country_filter", "start_year")])

print("=================== RUNNING run_prepare_prewrangled_financial_data_stress_test ===================")
source(fs::path("data-raw", "run_prepare_prewrangled_financial_data_stress_test.R"))
rm(list = ls()[ls() != c("country_filter", "start_year")])

# ===== SAVE TO DROPBOX


# Save data to dropbox only if no filter applied
if (length(country_filter) == 0) {
  for (fp in c(
    "abcd_stress_test_input.csv",
    "financial_data_stress_test.csv",
    "Scenarios_AnalysisInput.csv",
    "prewrangled_capacity_factors.csv",
    "price_data_long.csv",
    "ngfs_carbon_price.csv"
    )){
    
    readr::write_csv(
      readr::read_csv(here::here("data-raw", "st_inputs", fp)),
      r2dii.utils::path_dropbox_2dii(fs::path("ST Inputs", "ST_INPUTS_MASTER", fp))
    )
    
  }
} 
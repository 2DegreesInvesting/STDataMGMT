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



# ALIGN OPEN SOURCE DATAFRAMES TOGETHER

st_input_folder <- here::here("data-raw", "st_inputs")


st_data <- r2dii.climate.stress.test:::st_read_agnostic(
  st_input_folder,
  risk_type = "trisk"
  )

scenario_price <- st_data$scenario_data %>% 
  dplyr::inner_join(
    st_data$df_price,
    by = c("scenario", "ald_sector", "ald_business_unit", "year"),
    relationship="many-to-one" # many to one expected bc prices are not geography scpecific (Global)
    )

scenario_price_power <- scenario_price %>% dplyr::filter(ald_sector=="Power")
scenario_price_power_not_in_capfac <- scenario_price_power %>%
  dplyr::anti_join(
    st_data$capacity_factors_power ,
    by = c("scenario_geography", "scenario", "ald_business_unit"))

# remove from the scenarios+geographies which ones are not complete on all dataframes
available_scenario_geographies <- 
  scenario_price %>% 
  dplyr::distinct(scenario, scenario_geography) %>%
  dplyr::anti_join(
    scenario_price_power_not_in_capfac %>% 
    dplyr::distinct(scenario, scenario_geography)
    )


readr::read_csv(file.path(st_input_folder, "Scenarios_AnalysisInput.csv")) %>% 
  dplyr::inner_join(available_scenario_geographies) %>%
  readr::write_csv(file.path(st_input_folder, "Scenarios_AnalysisInput.csv"))
readr::read_csv(file.path(st_input_folder, "price_data_long.csv")) %>% 
  dplyr::inner_join(available_scenario_geographies %>% dplyr::distinct(scenario)) %>%
  readr::write_csv(file.path(st_input_folder, "price_data_long.csv"))
readr::read_csv(file.path(st_input_folder, "prewrangled_capacity_factors.csv"))  %>% 
  dplyr::inner_join(available_scenario_geographies %>% dplyr::distinct(scenario)) %>%
  readr::write_csv(file.path(st_input_folder, "prewrangled_capacity_factors.csv"))



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

# ===== TEST TRISK ON ALL COMBINATIONS OF SCENARIO/GEOGRAPHY


scenario_geography_x_ald_sector <- r2dii.climate.stress.test::get_scenario_geography_x_ald_sector(
  here::here("data-raw", "st_inputs")) |>
  dplyr::distinct(.data$baseline_scenario, .data$shock_scenario, .data$scenario_geography)

for (i in 1:nrow(scenario_geography_x_ald_sector)) {
  row_params <- scenario_geography_x_ald_sector[i, ]
  tryCatch({
    suppressWarnings(suppressMessages(capture.output(
      r2dii.climate.stress.test::run_trisk(
        input_path = st_input_folder,
        output_path = tempdir(),
        baseline_scenario = row_params$baseline_scenario,
        shock_scenario = row_params$shock_scenario,
        scenario_geography = row_params$scenario_geography
      )
    )))
          # cat(paste("Pass", row_params, "\n"))
    },
    error = function(e) {
      cat(e$message)
      cat(e$parent[1]$message)
      cat(paste("Failed", row_params, "\n"))
      NULL
      }
    )
}

# ===== SAVE TO DROPBOX


# Save data to dropbox only if no filter applied
if (length(country_filter) == 0) {
  for (fp in c(
    "abcd_stress_test_input.csv",
    "prewrangled_financial_data_stress_test.csv",
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
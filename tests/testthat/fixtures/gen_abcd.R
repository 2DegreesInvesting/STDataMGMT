devtools::load_all()

# ===================================
# ABCD STRESS TEST INPUT
# ===================================


start_year <- 2022
time_horizon <- 5
additional_year <- NULL
sector_list <- c("Automotive", "Power", "Oil&Gas", "Coal")
km_per_vehicle <- 15000

outputs_list <-
  prepare_assets_data(company_activities, company_emissions)

clean_company_activities <- outputs_list[["company_activities"]]
clean_company_emissions <- outputs_list[["company_emissions"]]

abcd_stress_test_input <-
  prepare_abcd_data(
    company_activities = clean_company_activities,
    company_emissions = clean_company_emissions,
    scenarios_geographies = scenarios_geographies,
    start_year = start_year,
    time_horizon = time_horizon,
    additional_year = additional_year,
    km_per_vehicle = km_per_vehicle,
    sector_list = sector_list
  ) %>%
  assertr::verify(all(colSums(is.na(.)) == 0))

usethis::use_data(abcd_stress_test_input)
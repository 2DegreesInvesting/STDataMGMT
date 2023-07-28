devtools::load_all()

## PARAMETERS
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

bench_regions <-
  readr::read_csv(here::here("data-raw", "bench_regions.csv"), na = "")

#### PREPROCESS ASSET RESOLUTION DATA

## DATALOAD
company_activities <-
  read_asset_resolution(path_ar_data_raw, sheet_name = "Company Activities")
company_emissions <-
  read_asset_resolution(path_ar_data_raw, sheet_name = "Company Emissions")

## TRANSFORM
company_activities <-
  pivot_equity_ownership_columns(company_activities)
company_emissions <-
  pivot_equity_ownership_columns(company_emissions)

## FILTERING
company_activities <-
  remove_unknown_owner_companies(company_activities)
company_emissions <-
  remove_unknown_owner_companies(company_emissions)

company_emissions <- remove_prop_emissions(company_emissions)

## RENAME
company_activities <- rename_technology(company_activities)
company_emissions <- rename_technology(company_emissions)

company_activities <- rename_ald_sector(company_activities)
company_emissions <- rename_ald_sector(company_emissions)

## AGGREGATIONS
company_activities <-
  aggregate_equity_ownership_after_renaming(company_activities)
company_emissions <-
  aggregate_equity_ownership_after_renaming(company_emissions)

###### ABCD

## DATALOAD
abcd_data <-
  match_emissions_to_production(company_activities, company_emissions)

## AGGREGATIONS

abcd_data <- aggregate_technology_types(abcd_data)
abcd_data <- fill_empty_years_that_follows(abcd_data)

# at this point, nans in ald_production are only due to fully empty production in raw data
# to check that, only 2 values with this command:
#   abcd_data %>% group_by(id, company_name, region, ald_location, ald_sector, technology, ald_production_unit, emissions_factor_unit) %>% summarise(nna=sum(is.na(ald_production))) %>% ungroup() %>% distinct(nna)

abcd_data <- recreate_prop_emissions(abcd_data)
abcd_data <- fill_missing_emission_factor(abcd_data)

# nans in emission_factor only on all years of a given thech (same as above)
# to check :
#  abcd_data %>% group_by(id, company_name, region, ald_location, ald_sector, technology, ald_production_unit, emissions_factor_unit) %>% summarise(nna=sum(is.na(emissions_factor))) %>% ungroup() %>% distinct(nna)

abcd_data <- drop_empty_prod_and_ef(abcd_data)
abcd_data <- expand_by_scenario_geography(abcd_data, bench_regions)

abcd_data <- create_plan_prod_columns(abcd_data)

## FILTERINGS
abcd_data <-
  filter_years_abcd_data(abcd_data, start_year, time_horizon, additional_year)


abcd_data <- filter_sectors_abcd_data(abcd_data, sector_list = c("HDV", "Automotive", "Power", "Fossil Fuels", "Oil&Gas", "Coal"))


abcd_data %>% readr::write_csv(output_path_stress_test_inputs)

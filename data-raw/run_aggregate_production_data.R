devtools::load_all()

path_pams_raw <- r2dii.utils::path_dropbox_2dii("ST_INPUTS", "ST_INPUTS_PRODUCTION", "2023-02-15_AI_2DII Germany-Company-Indicators_2022Q4 (1).xlsx")
company_activities <- readxl::read_xlsx(
  path_pams_raw,
  sheet = "Company Activities"
)
company_emissions <- readxl::read_xlsx(
  path_pams_raw,
  sheet = "Company Emissions"
)

output_path_stress_test_inputs <- fs::path(
  r2dii.utils::path_dropbox_2dii(), "ST_INPUTS", "ST_INPUTS_MASTER"
)

company_activities <- company_activities %>% dplyr::filter(`Company Name` != "Unknown Owner")
company_emissions <- company_emissions %>% dplyr::filter(`Company Name` != "Unknown Owner")

# functions

expand_tech_rows <- function(data, global_aggregate = FALSE) {
  na_list <- list(plan_tech_prod = 0, current_plan_row = 0)

  group_names <- c("id", "equity_market", "scenario_geography")
  if (global_aggregate) group_names <- c("scenario_source", "scenario", group_names)

  data <- dplyr::group_by(data, .data$ald_sector)
  data <-
    tidyr::complete(
      data,
      tidyr::nesting(!!!rlang::syms(group_names)),
      tidyr::nesting(technology),
      year,
      fill = na_list
    )
  dplyr::ungroup(data)
}

expand_by_scenario_geography <- function(data, scen_geos, bench_regions, .default = "Global", .iso2c = "ald_location") {
  stopifnot(.iso2c %in% names(data))
  stopifnot(all(scen_geos %in% bench_regions$scenario_geography))

  dict <-
    bench_regions %>%
    dplyr::filter(scenario_geography %in% scen_geos) %>%
    dplyr::select(country_iso, scenario_geography) %>%
    dplyr::distinct()

  data %>%
    dplyr::left_join(dict, by = setNames("country_iso", .iso2c)) %>%
    dplyr::mutate(scenario_geography = dplyr::case_when(
      is.na(scenario_geography) ~ .default,
      scenario_geography == "" ~ .default,
      TRUE ~ scenario_geography
    ))
}

expand_by_country_of_domicile <- function(data, index_regions, .default = "GlobalMarket", .iso2c = "country_of_domicile") {
  stopifnot(.iso2c %in% names(data))

  dict <- index_regions %>%
    dplyr::select(.data$equity_market, .data$country_iso) %>%
    dplyr::distinct()

  data %>%
    dplyr::left_join(dict, by = setNames("country_iso", .iso2c)) %>%
    dplyr::mutate(equity_market = dplyr::case_when(
      is.na(equity_market) ~ .default,
      equity_market == "" ~ .default,
      TRUE ~ equity_market
    ))
}

append_emissions_factor <- function(company_activities, company_emissions) {
  units_prod <- c(
    "MW", "dwt km", "pkm", "tkm", "GJ",
    "t coal", "t cement", "t steel", "# vehicles"
  )
  units_emission <- c(
    "tCO2e/MWh", "tCO2/dwt km", "tCO2e/t coal", "tCO2e/t cement",
    "tCO2/pkm", "tCO2e/GJ", "tCO2/tkm", "tCO2e/t steel", "tCO2/km"
  )

  emission_factors <- company_emissions %>%
    dplyr::filter(`Activity Unit` %in% units_emission) %>%
    dplyr::select(
      "Company ID", "Company Name", "Asset Sector",
      "Asset Technology", "Asset Technology Type",
      "Asset Region", "Asset Country",
      "Activity Unit", "Equity Ownership 2027"
    ) %>%
    dplyr::mutate(
      `Emissions Factor` = `Equity Ownership 2027`,
      `Emissions Factor Unit` = `Activity Unit`
    ) %>%
    dplyr::select(
      "Company ID", "Company Name", "Asset Sector",
      "Asset Technology", "Asset Technology Type",
      "Asset Region", "Asset Country",
      "Emissions Factor", "Emissions Factor Unit"
    )

  filtered_company_activities <- company_activities %>% dplyr::filter(`Activity Unit` %in% units_prod)
  company_activities_with_emission_factors <- dplyr::left_join(filtered_company_activities, emission_factors,
    by = c(
      "Company ID",
      "Company Name",
      "Asset Sector",
      "Asset Technology",
      "Asset Technology Type",
      "Asset Region",
      "Asset Country"
    )
  )
  company_activities_with_emission_factors
}

# prep

company_activities_with_emission_factors <- append_emissions_factor(company_activities, company_emissions)

abcd_data <- company_activities_with_emission_factors

# from data_prep_v2022:
# scenario prep options
scenario_sources_list <- c("WEO2021", "GECO2019")
sector_list <- c("HDV", "Automotive", "Power", "Fossil Fuels", "Oil&Gas", "Coal")
other_sector_list <- c("Shipping", "Steel", "Aviation", "Cement")
start_year <- 2021
time_horizon <- 5
additional_year <- NULL
relevant_years <- sort(unique(c(start_year:(start_year + time_horizon), additional_year)))

# For now, there is no global_aggregate provided in our scenario prep, so not needed
# running this code with global_aggregate = TRUE does not currently produce global aggregate outcomes
global_aggregate <- FALSE

# read scenario data
scenario_data <- readr::read_csv(here::here("data-raw", glue::glue("Scenarios_AnalysisInput_{start_year}.csv")))
scenario_data <- scenario_data[!(grepl("Cap", scenario_data$technology) & scenario_data$ald_sector == "Demand"), ]

# this should be based on the scenario.preparation repo as a single source of
# truth
bench_regions <- readr::read_csv(here::here("data-raw", "bench_regions.csv"))
index_regions <- readr::read_csv(here::here("data-raw", "bench_regions.csv"))

scenario_geographies_list <- scenario_data %>% dplyr::distinct(scenario_geography)

# filtering the available geographies from bench_regions
scenario_geographies_list <- scenario_geographies_list %>%
  dplyr::filter(scenario_geography %in% bench_regions$scenario_geography) %>%
  dplyr::pull()


# from data_prep_v2022:

# ABCD and scenario combined options
tech_exclude <-
  c(
    "OtherCap",
    "OtherFF",
    "Coking Plant",
    "Sintering Plant",
    "Direct Or Smelting Reduction Plant",
    "Pelletizing Plant",
    "Grinding Plant",
    "Passenger / Freight"
  )

global_aggregate_scenario_sources_list <- c("WEO2021", "GECO2019")
global_aggregate_sector_list <- c("Power")

abcd_data <- abcd_data %>%
  dplyr::select(-dplyr::starts_with("Direct Ownership")) %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("Equity Ownership "),
    names_to = "year",
    names_prefix = "Total ",
    values_to = "ald_production"
  ) %>%
  dplyr::mutate(year = stringr::str_extract(year, stringr::regex("\\d+"))) %>%
  dplyr::rename(
    id = .data$`Company ID`,
    company_name = .data$`Company Name`,
    ald_sector = .data$`Asset Sector`,
    technology = .data$`Asset Technology`,
    technology_type = .data$`Asset Technology Type`,
    ald_location = .data$`Asset Country`,
    emissions_factor = .data$`Emissions Factor`,
    emissions_factor_unit = .data$`Emissions Factor Unit`,
    ald_production_unit = .data$`Activity Unit`
  ) %>%
  dplyr::mutate(
    technology = dplyr::case_when(
      .data$ald_sector == "Coal" ~ "Coal",
      .data$technology %in% c("Gas", "Natural Gas Liquids") ~ "Gas",
      .data$technology == "Oil and Condensate" ~ "Oil",
      TRUE ~ .data$technology
    ),
    ald_sector = dplyr::if_else(
      .data$ald_sector == "LDV", "Automotive", .data$ald_sector
    )
  )

avg_emission_factors <- abcd_data %>%
  dplyr::group_by(.data$ald_sector, .data$technology, .data$technology_type, .data$emissions_factor_unit) %>%
  dplyr::summarise(
    emissions_factor = stats::weighted.mean(
      .data$emissions_factor, .data$ald_production,
      na.rm = TRUE
    )
  ) %>%
  dplyr::ungroup()

# use avg technology type EFs to fill missing values
abcd_missing_ef <- abcd_data %>%
  dplyr::filter(is.na(.data$emissions_factor))

abcd_missing_ef <- abcd_missing_ef %>%
  dplyr::select(-.data$emissions_factor, -.data$emissions_factor_unit) %>%
  dplyr::inner_join(
    avg_emission_factors,
    by = c("ald_sector", "technology", "technology_type")
  )

abcd_data <- abcd_data %>%
  dplyr::filter(!is.na(.data$emissions_factor)) %>%
  dplyr::bind_rows(abcd_missing_ef)


ald <- abcd_data %>%
  dplyr::select(
    ald_sector, technology, technology_type, year,
    # NOTE: country of domicile not included in Stress test version. This
    # refers to company headquarters and must be obtained from financial data.
    id, ald_location, ald_production, ald_production_unit, emissions_factor
  ) %>%
  dplyr::filter(
    year %in% c(start_year:(start_year + time_horizon), additional_year),
    !technology %in% tech_exclude,
    # The exclusion sometimes seem to be technology types, added this for stress test
    !technology_type %in% tech_exclude,
    # Done in stress test, because we only consider sectors with prod pathways
    ald_sector %in% sector_list
  ) %>%
  dplyr::mutate(
    ald_sector = dplyr::case_when(
      technology == "Coal" ~ "Coal",
      technology %in% c("Gas", "Oil") ~ "Oil&Gas",
      TRUE ~ ald_sector
    )
  )

# TODO: country_ald may not be needed for stress test
# Create a country production dataset for the maps:
country_ald <- ald %>%
  dplyr::mutate(
    ald_production = dplyr::case_when(
      technology == "Oil" ~ ald_production * (1 / 6.12), # FIXME
      technology == "Gas" ~ ald_production * (1 / 0.0372), # FIXME
      TRUE ~ ald_production
    ),
    ald_production_unit = dplyr::case_when(
      technology == "Oil" ~ "boe",
      technology == "Gas" ~ "MMCFD",
      TRUE ~ ald_production_unit
    )
  ) %>%
  dplyr::distinct(id, ald_location, ald_sector, technology, year, ald_production, ald_production_unit)

if (global_aggregate == TRUE) {
  scenario_data <- scenario_data %>%
    tidyr::separate(col = scenario, into = c("scenario_source", "rest"), sep = "_", remove = FALSE) %>%
    dplyr::select(-.data$rest) %>%
    dplyr::filter(scenario_source %in% global_aggregate_scenario_sources_list)

  ald_ga_input <- bench_regions %>%
    dplyr::select(scenario_geography, country_iso, reg_count) %>%
    unique() %>%
    dplyr::left_join(
      scenario_data %>% dplyr::select(scenario_source, scenario, scenario_geography, ald_sector) %>% unique(),
      by = "scenario_geography"
    ) %>%
    dplyr::group_by(scenario_source, scenario, ald_sector, country_iso) %>%
    dplyr::mutate(rank = rank(reg_count, ties.method = "first")) %>%
    dplyr::filter(rank == 1, scenario_source %in% global_aggregate_scenario_sources_list) %>%
    dplyr::select(-reg_count, -rank)

  ald_sr <- ald %>%
    dplyr::filter(
      ald_sector %in% global_aggregate_sector_list
    ) %>%
    dplyr::left_join(ald_ga_input, by = c("ald_location" = "country_iso", "ald_sector" = "ald_sector")) %>%
    dplyr::mutate(
      scenario_geography = ifelse(
        is.na(scenario_geography) | scenario_geography == "",
        "Global",
        scenario_geography
      )
    )
} else {
  ald_sr <- expand_by_scenario_geography(ald, scenario_geographies_list, bench_regions)
}

ald_sr_ir <- ald_sr %>% dplyr::mutate(equity_market = "GlobalMarket")

if (global_aggregate == TRUE) {
  ald_sr_ir_agg <- ald_sr_ir %>%
    dplyr::group_by(
      scenario_source, scenario, id, equity_market, scenario_geography, ald_sector, technology, year
    ) %>%
    dplyr::summarise(
      plan_emission_factor = stats::weighted.mean(emissions_factor, ald_production, na.rm = TRUE),
      plan_tech_prod = sum(ald_production, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      current_plan_row = 1
    ) %>%
    dplyr::ungroup()
} else {
  ald_sr_ir_agg <- ald_sr_ir %>%
    dplyr::group_by(
      id, equity_market, scenario_geography, ald_sector, technology, year
    ) %>%
    dplyr::summarise(
      plan_emission_factor = stats::weighted.mean(emissions_factor, ald_production, na.rm = TRUE),
      plan_tech_prod = sum(ald_production, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(current_plan_row = 1)
}


### ########################################################################## #
### ADD THE EXTRA TECHNOLOGY LINES -----
### ########################################################################## #

# ald_full <- ald_sr_ir_agg
ald_full <- expand_tech_rows(ald_sr_ir_agg, global_aggregate = global_aggregate)

# if we get an NaN for the weighted mean of the EF, we set it to 0 in case
# the production is also 0. Since we only use the EF in the product of
# EF and production, this will have no impact on later calculations of
# absolute emissions
ald_full <- ald_full %>%
  dplyr::mutate(
    plan_emission_factor = dplyr::if_else(
      .data$plan_tech_prod == 0,
      0,
      .data$plan_emission_factor
    )
  )

pams_names <- company_activities %>%
  dplyr::distinct(`Company ID`, `Company Name`) %>%
  dplyr::rename(
    id = `Company ID`,
    company_name = `Company Name`
  )

abcd_full <- ald_full %>%
  dplyr::inner_join(pams_names, by = "id") %>%
  dplyr::select(
    id, company_name, scenario_geography, year, ald_sector, technology,
    plan_tech_prod, plan_emission_factor
  ) %>%
  dplyr::group_by(id, company_name, scenario_geography, year, ald_sector) %>%
  dplyr::mutate(plan_sec_prod = sum(plan_tech_prod, na.rm = TRUE)) %>%
  dplyr::ungroup()

abcd_full %>%
  readr::write_csv(
    file.path(
      output_path_stress_test_inputs, "abcd_stress_test_input.csv"
    )
  )

devtools::load_all()

# make sure to set the relevant start year when running the data preparation
# example:
 start_year <- 2021
input_path <- r2dii.utils::path_dropbox_2dii(
  "PortCheck",
  "00_Data",
  "01_ProcessedData",
  "03_ScenarioData",
  glue::glue("new_Scenarios_AnalysisInput_{start_year}.csv")
)

data <- readr::read_csv(
  input_path,
  col_types = readr::cols_only(
    Source = "c",
    Technology = "c",
    ScenarioGeography = "c",
    Sector = "c",
    Units = "c",
    Indicator = "c",
    Scenario = "c",
    Sub_Technology = "c",
    Year = "d",
    Direction = "c",
    mktFSRatio = "d",
    techFSRatio = "d",
    FairSharePerc = "d"
  )
)

prepared_data <- prepare_scenario_data(data = data, start_year = start_year)


# due to likely manual errors the raw input scenario data for start year 2020
# versus 2021, there are slight formatting differences in a scenario geography
# Fixing this here in a hardcoded way.
if (start_year == 2021) {
  prepared_data <- prepared_data %>%
    dplyr::mutate(
      scenario_geography = dplyr::if_else(
        .data$scenario_geography == "Central&SouthAmerica",
        "CentralandSouthAmerica",
        .data$scenario_geography
      )
    ) %>%
    dplyr::mutate(
      scenario_geography = dplyr::if_else(
        .data$scenario_geography == "Emerging market and developing economies",
        "Emergingmarket&developingeconomies",
        .data$scenario_geography
      )
    ) %>%
    dplyr::mutate(
      scenario_geography = dplyr::if_else(
        .data$scenario_geography == "EU27",
        "EU",
        .data$scenario_geography
      )
    ) %>%
    dplyr::mutate(
      scenario_geography = dplyr::if_else(
        .data$scenario_geography == "Advanced economies",
        "AdvancedEconomies",
        .data$scenario_geography
      ))
}

#filter for the automotive sector for the GECO data
if (start_year == 2021) {
geco_power_sector <- prepared_data %>% filter(str_detect(scenario, 'GECO')) %>% filter(ald_sector != "Automotive")

prepared_data <- prepared_data %>% anti_join(geco_power_sector)
}

prepared_data %>% readr::write_csv(
  file.path("data-raw", glue::glue("Scenarios_AnalysisInput_{start_year}.csv"))
)

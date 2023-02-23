devtools::load_all()

# make sure to set the relevant start year when running the data preparation
# example:
start_year <- 2021
input_path <- r2dii.utils::path_dropbox_2dii(
  "PortCheck",
  "00_Data",
  "01_ProcessedData",
  "03_ScenarioData",
  glue::glue("new_shorter_Scenarios_AnalysisInput_{start_year}.csv")
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


# due to likely manual errors the raw input scenario data for start year 2021
# versus before, there are slight formatting differences in a scenario geography
# Fixing this here in a hardcoded way.
if (start_year == 2021) {
  prepared_data <- prepared_data %>%
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
      )
    )
}

# make sure to set the relevant start year when running the data preparation
# example:
start_year <- 2021
input_path <- r2dii.utils::path_dropbox_2dii(
  "PortCheck",
  "00_Data",
  "01_ProcessedData",
  "03_ScenarioData",
  glue::glue("ngfs_Scenarios_AnalysisInput_{start_year}.csv")
)

data <- readr::read_csv(
  input_path,
  col_types = readr::cols_only(
    Model = "c",
    Scenario = "c",
    Region = "c",
    Variable = "c",
    category_a = "c",
    category_b = "c",
    category_c = "c",
    Unit = "c",
    year = "d",
    value = "d"
  )
)

preprepared_ngfs_data <- preprepare_ngfs_scenario_data(data)

# scenario values will be linearly interpolated for each group below
interpolation_groups <- c(
  "source",
  "scenario",
  "sector",
  "technology",
  "scenario_geography",
  "indicator",
  "units"
)

preprepared_ngfs_data <- preprepared_ngfs_data %>%
  interpolate_yearly(!!!rlang::syms(interpolation_groups)) %>%
  dplyr::filter(year >= start_year) %>%
  add_market_share_columns(start_year = start_year)

# vector of green technolgies
green_techs <- c(
  "FuelCell",
  "Electric",
  "Hybrid",
  "RenewablesCap",
  "HydroCap",
  "NuclearCap",
  "FuelCell_HDV",
  "Electric_HDV",
  "Hybrid_HDV"
)

preprepared_ngfs_data <- preprepared_ngfs_data %>% format_p4i(green_techs)

prepared_data_combined <- dplyr::full_join(prepared_data, preprepared_ngfs_data)

prepared_data_combined %>% readr::write_csv(
  file.path("data-raw", glue::glue("Scenarios_AnalysisInput_{start_year}.csv"))
)

### IPR Scenario
### Read IPR


start_year <- 2021
input_path <- r2dii.utils::path_dropbox_2dii(
  "PortCheck",
  "00_Data",
  "01_ProcessedData",
  "03_ScenarioData",
  glue::glue("ipr_Scenarios_AnalysisInput_2021.csv")
)

IPR <- as.data.frame(readr::read_csv(
  input_path
))

prepared_IPR_data <- prepare_IPR_scenario_data(IPR)

### Merge Data from Scenario Sources
prepared_data_IEA_NGFS <- dplyr::full_join(prepared_data, preprepared_ngfs_data)
prepared_data_combined <- dplyr::full_join(prepared_data_IEA_NGFS, prepared_IPR_data)

prepared_data_combined %>% readr::write_csv(
  file.path("data-raw", glue::glue("Scenarios_AnalysisInput_{start_year}.csv"))
)

devtools::load_all()

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
  "units",
  "model"
)

preprepared_ngfs_data <- preprepared_ngfs_data %>% interpolate_yearly(!!!rlang::syms(interpolation_groups))

preprepared_ngfs_data <- preprepared_ngfs_data %>% dplyr::filter(year >= start_year)

preprepared_ngfs_data <- preprepared_ngfs_data %>% add_market_share_columns(start_year = start_year)

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

preprepared_ngfs_data %>% readr::write_csv(
  file.path("data-raw", glue::glue("preprepared_NGFS_Scenarios_AnalysisInput_{start_year}.csv"))
)

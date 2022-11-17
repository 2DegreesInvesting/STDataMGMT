devtools::load_all()
library(pointblank)



input_path_scenario_analysisinput <- path_dropbox_2dii(
  "ST_INPUTS",
  "ST_INPUTS_MASTER",
  "weo2021_manually_added_Scenarios_AnalysisInput_2021.csv"
)

scenario_analysisinput <- readr::read_csv(
  file.path(input_path_scenario_analysisinput)
)

agent_scenario_analysisinput <- scenario_analysisinput |>
  create_agent(actions = action_levels(warn_at = 1, stop_at = 2))

plan_agent_scenario_analysisinput <- agent_scenario_analysisinput |>
  col_vals_not_null(columns = c(
    "scenario_geography",
    "scenario",
    "ald_sector",
    "units",
    "technology",
    "year",
    "direction",
    "fair_share_perc"
  )) |>
  col_is_numeric(columns = c(
    "fair_share_perc",
    "year"
  )) |>
  col_is_character(columns = c(
    "scenario_geography",
    "ald_sector",
    "scenario",
    "units",
    "technology",
    "direction"
  )) |>
  rows_distinct(columns = c(
    "scenario",
    "scenario_geography",
    "year",
    "ald_sector",
    "technology",
    "units"
  ))

plan_agent_scenario_analysisinput |>
  interrogate() |>
  export_report(filename = affix_datetime(
    "./test_validate_data_sets/report_weo2021_manually_added_scenarios_analysisinput_2021.html"
  ))

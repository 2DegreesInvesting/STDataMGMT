devtools::load_all()
library(pointblank)

input_path_prewrangled_capacity <- path_dropbox_2dii(
  "ST_INPUTS",
  "ST_INPUTS_MASTER",
  "prewrangled_capacity_factors.csv"
)

prewrangled_capacity_factors <- readr::read_csv(
  file.path(input_path_prewrangled_capacity)
)

agent_prewrangled_capacity_factors <- prewrangled_capacity_factors |>
  create_agent(actions = action_levels(warn_at = 1, stop_at = 2))

plan_prewrangled_capacity_factors <- agent_prewrangled_capacity_factors |>
  col_vals_not_null(columns = c(
    "year",
    "scenario",
    "scenario_geography",
    "technology",
    "capacity_factor"
  )) |>
  col_is_numeric(columns = c(
    "capacity_factor",
    "year"
  )) |>
  col_is_character(columns = c(
    "scenario",
    "scenario_geography",
    "technology"
  )) |>
  rows_distinct(columns = c(
    "year",
    "scenario",
    "scenario_geography",
    "technology"
  ))


plan_prewrangled_capacity_factors |>
  interrogate() |>
  export_report(filename = affix_datetime(
    "./test_validate_data_sets/report_prewrangled_capacity_factors.html"
  ))

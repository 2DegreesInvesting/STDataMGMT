devtools::load_all()
library(pointblank)


input_path_price_data_long <- path_dropbox_2dii(
  "ST_INPUTS",
  "ST_INPUTS_MASTER",
  "price_data_long.csv"
)

price_data_long <- readr::read_csv(
  file.path(input_path_price_data_long)
)


agent_price_data_long <- price_data_long |>
  create_agent(actions = action_levels(warn_at = 1, stop_at = 2))

plan_price_data_long <- agent_price_data_long |>
  col_vals_not_null(columns = c(
    "year",
    "scenario",
    "technology",
    "indicator",
    "unit",
    "price"
  )) |>
  col_is_numeric(columns = c(
    "price",
    "year"
  )) |>
  col_is_character(columns = c(
    "scenario",
    "sector",
    "scenario_geography",
    "technology",
    "indicator",
    "unit"
  )) |>
  rows_distinct(columns = c(
    "year",
    "scenario",
    "technology",
    "sector",
    "scenario_geography"
  ))

plan_price_data_long |>
  interrogate() |>
  export_report(filename = affix_datetime(
    "./test_validate_data_sets/report_price_data_long.html"
  ))

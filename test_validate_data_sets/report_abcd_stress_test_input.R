input_path_abcd_stress_test <- path_dropbox_2dii(
  "ST_INPUTS",
  "ST_INPUTS_MASTER",
  "abcd_stress_test_input.csv"
)

abcd_stress_test <- readr::read_csv(
  file.path(input_path_abcd_stress_test)
)

agent_abcd_stress_test_input <- abcd_stress_test |>
  create_agent(actions = action_levels(warn_at = 1, stop_at = 2))

plan_abcd_stress_test_input <- agent_abcd_stress_test_input |>
  col_vals_not_null(columns = c(
    "id",
    "company_name",
    "scenario_geography",
    "year",
    "ald_sector",
    "technology",
    "plan_tech_prod",
    "plan_sec_prod"
  )) |>
  col_is_numeric(columns = c(
    "id",
    "plan_tech_prod",
    "plan_emission_factor",
    "plan_sec_prod",
    "year"
  )) |>
  col_is_character(columns = c(
    "company_name",
    "scenario_geography",
    "ald_sector",
    "technology"
  )) |>
  rows_distinct(columns = c(
    "id",
    "company_name",
    "scenario_geography",
    "year",
    "ald_sector",
    "technology"
  ))

plan_abcd_stress_test_input |>
  interrogate() |>
  export_report(filename = affix_datetime(
    "./test_validate_data_sets/report_abcd_stress_test_input.html"
  ))

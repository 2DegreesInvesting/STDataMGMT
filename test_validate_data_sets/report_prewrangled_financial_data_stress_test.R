devtools::load_all()
library(pointblank)

input_path_prewrangled_financial <- path_dropbox_2dii(
  "ST_INPUTS",
  "ST_INPUTS_MASTER",
  "prewrangled_financial_data_stress_test.csv"
)

prewrangled_financial_data_stress_test <- readr::read_csv(
  file.path(input_path_prewrangled_financial)
)

agent_prewrangled_financial_data_stress_test <- prewrangled_financial_data_stress_test |>
  create_agent(actions = action_levels(warn_at = 1, stop_at = 2))


plan_prewrangled_financial_data_stress_test <- agent_prewrangled_financial_data_stress_test |>
  col_vals_not_null(columns = c(
    "company_name",
    "company_id",
    "pd",
    "net_profit_margin",
    "debt_equity_ratio",
    "volatility"
  )) |>
  col_is_numeric(columns = c(
    "company_id",
    "pd",
    "net_profit_margin",
    "debt_equity_ratio",
    "volatility"
  )) |>
  col_is_character(columns = c(
    "company_name",
    "corporate_bond_ticker"
  )) |>
  rows_distinct(columns = c(
    "company_name",
    "company_id",
    "corporate_bond_ticker"
  ))

plan_prewrangled_financial_data_stress_test |>
  interrogate() |>
  export_report(filename = affix_datetime(
    "./test_validate_data_sets/report_prewrangled_financial_data_stress_test.html"
  ))

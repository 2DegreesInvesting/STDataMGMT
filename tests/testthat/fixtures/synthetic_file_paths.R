#' @export
synth_fp <- list(
  company_activities = file.path("data-raw", "synthetic_inputs", "company_activities.parquet"),
  company_emissions = file.path("data-raw", "synthetic_inputs", "company_emissions.parquet"),
  eikon_data = file.path("data-raw", "synthetic_inputs", "eikon_data.parquet"),
  abcd_stress_test_input = file.path("data-raw", "synthetic_inputs", "abcd_stress_test_input.parquet"),
  prewrangled_financial_data_stress_test = file.path("data-raw", "synthetic_inputs", "prewrangled_financial_data_stress_test.parquet")
)

devtools::load_all()

# requires eikon_data
# this can be obtained by running run_prepare_eikon_data
# without saving the output file

# prepare financial data stress test--------------------------------
list_prewrangled_financial_data <- eikon_data %>%
  prepare_prewrangled_financial_data_stress_test()

list_prewrangled_financial_data[[1]] %>%
  readr::write_csv(
    file.path("data-raw", "prewrangled_financial_data_stress_test.csv")
  )

# optional - exporting qa files for removed profit margins
# path_fin_data_st_rm_npm <- file.path(output_path_db_analysis_inputs, "financial_data_stress_test_rm_npm.csv")
# list_prewrangled_financial_data[[2]] %>%
#   readr::write_csv(path_fin_data_st_rm_npm)

# optional - exporting qa files for ascii character
# financial_data_stress_test_rm_non_ascii <- file.path(output_path_db_analysis_inputs, "financial_data_stress_test_rm_non_ascii.csv")
# list_prewrangled_financial_data[[3]] %>%
#   readr::write_csv(financial_data_stress_test_rm_non_ascii)

dir.create(path=fs::path("data-raw", "synthetic_inputs"),showWarnings = F )

print("=================== RUNNING gen_synthetic_company_data ===================")
source(fs::path("tests", "testthat", "fixtures", "gen_synthetic_company_data.R"))
rm(list = ls())

print("=================== RUNNING gen_synthetic_abcd ===================")
source(fs::path("tests", "testthat", "fixtures", "gen_synthetic_abcd.R"))
rm(list = ls())

print("=================== RUNNING gen_synthetic_eikon_data ===================")
source(fs::path("tests", "testthat", "fixtures", "gen_synthetic_eikon_data.R"))
rm(list = ls())

print("=================== RUNNING gen_synthetic_financial_data ===================")
source(fs::path("tests", "testthat", "fixtures", "gen_synthetic_financial_data.R"))
rm(list = ls())



box::use(
  tests/ testthat / fixtures/ synthetic_file_paths[synth_fp],
)

  synthetic_company_activities  <-   arrow::read_parquet(synth_fp$company_activities)
  synthetic_company_emissions <- arrow::read_parquet(synth_fp$company_emissions)
  synthetic_eikon_data <- arrow::read_parquet(synth_fp$eikon_data)

  usethis::use_data(synthetic_company_activities, overwrite=TRUE)
  usethis::use_data(synthetic_company_emissions, overwrite=TRUE)
  usethis::use_data(synthetic_eikon_data, overwrite=TRUE)
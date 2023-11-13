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



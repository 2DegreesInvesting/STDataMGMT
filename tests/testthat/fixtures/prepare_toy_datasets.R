devtools::load_all()

synthetic_data_source <- here::here("tests","testthat","fixtures")

company_activities <- readr::read_rds("tests/testthat/fixtures/company_activities.rds")
company_emissions <- readr::read_rds("tests/testthat/fixtures/company_emissions.rds")


start_year <- 2022
time_horizon <- 5
additional_year <- NULL
sector_list <- c("Automotive", "Power", "Oil&Gas", "Coal")
km_per_vehicle <- 15000

bench_regions <-
  readr::read_rds(here::here("data-raw", "bench_regions.rds"))

company_activities <-
  read_asset_resolution(
    fs::path(path_ar_data_raw,
      "AR-Company-Indicators_2022Q4",
      ext = "xlsx"
    ),
    sheet_name = "Company Activities"
  )
company_emissions <-
  read_asset_resolution(
    fs::path(path_ar_data_raw,
      "AR-Company-Indicators_2022Q4",
      ext = "xlsx"
    ),
    sheet_name = "Company Emissions"
  )

outputs_list <-
  prepare_assets_data(company_activities, company_emissions)

clean_company_activities <- outputs_list[["company_activities"]]
clean_company_emissions <- outputs_list[["company_emissions"]]

abcd_data <-
  prepare_abcd_data(
    company_activities = clean_company_activities,
    company_emissions = clean_company_emissions,
    scenarios_geographies = bench_regions,
    start_year = start_year,
    time_horizon = time_horizon,
    additional_year = additional_year,
    km_per_vehicle = km_per_vehicle,
    sector_list = sector_list
  )

abcd_data %>%
  assertr::verify(all(colSums(is.na(.)) == 0))

abcd_sample <- readr::read_rds(
    
)
financial_sample <- readr::read_rds(

)

path_ar_data_raw <-
  r2dii.utils::path_dropbox_2dii(
    "ST_INPUTS",
    "ST_INPUTS_PRODUCTION"
  )
company_activities <-
  read_asset_resolution(
    fs::path(path_ar_data_raw,
      "AR-Company-Indicators_2022Q4",
      ext = "xlsx"
    ),
    sheet_name = "Company Activities"
  )
company_emissions <-
  read_asset_resolution(
    fs::path(path_ar_data_raw,
      "AR-Company-Indicators_2022Q4",
      ext = "xlsx"
    ),
    sheet_name = "Company Emissions"
  )

# PICK SOME COMPANIES BY ID

some_ids <- c(919, 5673, 12131, 14688, 151809)

financial_sample <- financial_sample %>%
  filter(company_id %in% some_ids)
abcd_sample <- abcd_sample %>%
  filter(id %in% some_ids) %>%
  filter(ald_sector %in% c("Coal", "Oil&Gas", "Power")) %>%
  filter(scenario_geography == "Global")

company_activities_sample <- company_activities %>%
  filter(id %in% some_ids) %>%
  filter(ald_sector %in% c("Coal", "Oil&Gas", "Power"))

company_emissions_sample <- company_emissions %>%
  filter(id %in% some_ids) %>%
  filter(ald_sector %in% c("Coal", "Oil&Gas", "Power"))

## ALIGN COMPANY NAMES

financial_sample <- financial_sample %>%
  select(c(-company_name)) %>%
  inner_join(abcd_sample %>% distinct(id, company_name),
    by = c("company_id" = "id")
  )

## ANONYMIZE DATA

hex_to_int <- function(h) {
  xx <- strsplit(tolower(h), "")[[1L]]
  pos <- match(xx, c(0L:9L, letters[1L:6L]))
  sum((pos - 1L) * 16^(rev(seq_along(xx) - 1)))
}

hash_in_range <- function(x) {
  # ## Compute md5 hash of R representation of each input number
  # (sapply(x, digest))
  # # [1] "a276b4d73a46e5a827ccc1ad970dc780" "328dd60879c478d49ee9f3488d71a0af"
  # # [3] "e312c7f09be7f2e8391bee2b85f77c11" "e4ac99a3f0a904b385bfdcd45aca93e5"
  # # [5] "470d800a40ad5bc34abf2bac4ce88f37" "0008f4edeebbafcc995f7de0d5c0e5cb"
  #
  # ## Only really need the last few hex digits
  # substr(sapply(x, digest), 28, 32)
  # # [1] "dc780" "1a0af" "77c11" "a93e5" "88f37" "0e5cb"
  #
  # ## Convert hex strings to decimal integers
  # strtoi(substr(sapply(x, digest), 28, 32), 16L)
  # # [1] 903040 106671 490513 693221 560951  58827
  #
  # ## Map those to range between 0 and 999
  # strtoi(substr(sapply(x, digest), 28, 32), 16L) %% 1e3
  # # [1]  40 671 513 221 951 827

  return(strtoi(substr(digest::digest(x), 28, 32), 16L) %% 1e6)
}


financial_sample <- financial_sample %>%
  rowwise() %>%
  mutate(
    company_name = rlang::hash(company_name),
    company_id = hash_in_range(company_id)
  ) %>%
  select(company_id, company_name, corporate_bond_ticker, pd, net_profit_margin, debt_equity_ratio, volatility)

company_activities_sample <- company_activities_sample %>%
  rowwise() %>%
  mutate(
    company_name = rlang::hash(company_name),
    id = hash_in_range(id)
  )
company_emissions_sample <- company_emissions_sample %>%
  rowwise() %>%
  mutate(
    company_name = rlang::hash(company_name),
    id = hash_in_range(id)
  ) %>%
  filter(activity_unit %in% c("tCO2e", "tCO2"))

abcd_sample <- abcd_sample %>%
  rowwise() %>%
  mutate(
    company_name = rlang::hash(company_name),
    id = hash_in_range(id)
  )


## SAVE RESULTS

outdir <- file.path("~/2° Investing Dropbox/Bertrand Gallice/ST_INPUTS/indonesia_sample")
# outdir <- here::here("st_inputs_test/ST input samples anonymized/")
financial_sample %>% readr::write_csv(file.path(outdir, "prewrangled_financial_data_stress_test.csv"))
company_activities_sample %>% readr::write_csv(file.path(outdir, "company_activities.csv"))
company_emissions_sample %>% readr::write_csv(file.path(outdir, "company_emissions.csv"))
abcd_sample %>% readr::write_csv(file.path(outdir, "abcd_stress_test_input.csv"))

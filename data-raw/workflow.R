devtools::load_all()

# TODO
#   - prewrangled_capacity_factors
#   - price_data_long
#   - Scenarios_AnalysisInput

## PARAMETERS
# TODO config file

#### bench_regions renaming
matching_tol <- 1

input_bench_regions_path <-
  output_bench_regions_path <-
  here::here("data-raw", "bench_regions.csv")

path_prewrangled_capacity_factors <-
  here::here("data-raw", "prewrangled_capacity_factors.csv")
path_price_data_long <-
  here::here("data-raw", "price_data_long.csv")
path_Scenarios_AnalysisInput <-
  here::here("data-raw", "Scenarios_AnalysisInput_2021.csv")

#### abcd_stress_test_input
path_ar_data_raw <-
  r2dii.utils::path_dropbox_2dii("ST_INPUTS",
                                 "ST_INPUTS_PRODUCTION")

output_path_stress_test_input <-
  r2dii.utils::path_dropbox_2dii("ST_INPUTS",
                                 "ST_INPUTS_MASTER")

start_year <- 2021
time_horizon <- 5
additional_year <- NULL
sector_list <- c("Automotive", "Power", "Oil&Gas", "Coal")


#### eikon data
# 1) file locations-----
# the input files read into and output files generated with this script are
# rather large in size and cannot be stored within the repo.

# master_data for different asset types and financial data from bbg and eikon are
# read in from the drop box, indicated by file.paths
# - path_db_analysis_inputs
# - path_db_datastore
# - path_db_eikon_data

# output files are stored in the dropbox in a directory indicated by file.path
# - path_db_analysis_inputs
# - generated with stresstest_masterdata_files(), which normally points to the
# same directory as path_db_analysis_inputs

path_db_analysis_inputs <-
  fs::path(r2dii.utils::dbox_port_00(), "07_AnalysisInputs", "2023Q2")
output_path_db_analysis_inputs <-
  fs::path(r2dii.utils::dbox_port_00(),
           "07_AnalysisInputs",
           "2020Q4_05182021_2020")
path_db_datastore <- fs::path(
  r2dii.utils::dbox_port_00(),
  "06_DataStore",
  "DataStore_export_05172021",
  "2020Q4"
)
path_db_eikon_data <- fs::path(r2dii.utils::dbox_port_00(),
                               "02_FinancialData",
                               "Eikon_Data",
                               "2023Q2")

# 2) set parameters------
# regional level for calculating financial data averages
level_subregion <- "REGION"

# parameters for minimum requirements to reference subgroups in creating averages
# determine size of subgroup below which we do not use the average because the
# minimum required sample size of reference subgroup
n_min_sample <- 50
# minimum required ratio of reference subgroup to sample
min_ratio_sample_subgroup <- 1 / 3
# cut off values for profit margins
range_profit_margin <- c(-Inf, Inf)




## MAIN

#### rename bench_regions
bench_regions <-
  readr::read_csv(input_bench_regions_path, na = c(""))

output_list <- regroup_and_rename_geographies(
  bench_regions = bench_regions,
  path_prewrangled_capacity_factors = path_prewrangled_capacity_factors,
  path_price_data_long = path_price_data_long,
  path_Scenarios_AnalysisInput = path_Scenarios_AnalysisInput,
  matching_tol = matching_tol
)

trisk_input_dfs_renamed <- output_list[["trisk_input_dfs"]]
bench_regions_renamed <- output_list[["bench_regions"]]

## write results
bench_regions %>% readr::write_csv(output_bench_regions_path, na = c(""))

for (fp in names(trisk_input_dfs_renamed)) {
  readr::write_csv(trisk_input_dfs_renamed[[fp]], fp)
}


rm(output_list, trisk_input_dfs_renamed)

### abcd_stress_test_input
company_activities <-
  read_asset_resolution(fs::path(path_ar_data_raw,
                                 "AR-Company-Indicators",
                                 ext = "xlsx"),
                        sheet_name = "Company Activities")
company_emissions <-
  read_asset_resolution(fs::path(path_ar_data_raw,
                                 "AR-Company-Indicators",
                                 ext = "xlsx"),
                        sheet_name = "Company Emissions")

output_list <-
  prepare_assets_data(company_activities, company_emissions)

clean_company_activities <- output_list[["company_activities"]]
clean_company_emissions <- output_list[["company_emissions"]]

rm(output_list, company_activities, company_emissions)

abcd_data <-
  prepare_abcd_data(
    company_activities = clean_company_activities,
    company_emissions = clean_company_emissions,
    scenarios_geographies = bench_regions_renamed,
    start_year = start_year,
    time_horizon = time_horizon,
    additional_year = additional_year,
    sector_list = sector_list
  )

rm(clean_company_emissions, clean_company_activities)

## write results
abcd_data %>% readr::write_csv(fs::path(
  output_path_stress_test_input,
  "abcd_stress_test_input",
  ext = "csv"
))



#### eikon data

# 3) load input data----------------------------------------------------

# security financial data--------
# read from the dropbox. produced by running the data_preparation repo
security_financial_data <-
  readr::read_rds(fs::path(path_db_analysis_inputs, "security_financial_data", ext = "rda"))
# add complementary isins/company_id from security_financial_data
asset_impact_isins <- readxl::read_excel(fs::path(path_db_analysis_inputs, "AR-Company-Indicators.xlsx"),
                                         sheet = "Company ISINs")  %>%
  dplyr::rename(company_id = `Company ID`,
                company_name = `Company Name`,
                isin=ISIN) %>%
  dplyr::mutate(security_mapped_sector="Other")
security_financial_data <-
  dplyr::bind_rows(
    security_financial_data %>% dplyr::distinct(company_id, company_name, isin, security_mapped_sector),
    asset_impact_isins
  )
# consolidated financial data----
# read from the dropbox. produced by running the data_preparation repo
consolidated_financial_data <-
  readr::read_rds(fs::path(path_db_analysis_inputs, "consolidated_financial_data", ext = "rda"))

# ownership_tree-----------------
# read from the dropbox. file provided by AR. clarify interval of releases.
ownership_tree <- readr::read_csv(
  file.path(path_db_datastore, "company_ownership_bidirectional.csv"),
  col_types = readr::cols_only(
    target_company_id = "d",
    company_id = "d",
    linking_stake = "d",
    ownership_level = "d"
  )
)

# make sure ownership structures are unique
ownership_tree <- ownership_tree %>%
  dplyr::distinct_all() %>%
  report_diff_rows(initial_n_rows = nrow(ownership_tree),
                   cause = "by ensuring column structure is unique")

# country region bridge-------
country_region_bridge <- rworldmap::countryRegions %>%
  dplyr::rename(iso_a3 = ISO3) %>%
  dplyr::inner_join(ISOcodes::ISO_3166_1, by = c("iso_a3" = "Alpha_3")) %>%
  dplyr::rename(iso_a2 = Alpha_2) %>%
  dplyr::select(iso_a2, iso_a3, everything()) %>%
  dplyr::distinct(iso_a2, .keep_all = TRUE) %>%
  dplyr::as_tibble()

# choose which region should be used
country_region_bridge <- country_region_bridge %>%
  dplyr::transmute(iso_a2, subregion = !!rlang::sym(level_subregion)) %>%
  # dplyr::transmute(iso_a2, subregion = REGION) %>%
  tidyr::drop_na()

# load raw eikon financial data ---------------------------------------------------

eikon_data_input <-
  readr::read_csv(fs::path(path_db_eikon_data, "eikon_data.csv"))

# load asset_impact data -------
# asset_impact_company_informations
asset_impact_data <-
  readxl::read_excel(fs::path(path_db_analysis_inputs, "AR-Company-Indicators.xlsx"),
                     sheet = "Company Information") %>%
  dplyr::select(c(-LEI)) %>%
  dplyr::rename(
    company_id = `Company ID`,
    company_name = `Company Name`,
    is_ultimate_parent = `Is Ultimate Parent`,
    country_of_domicile = `Country of Domicile`
  )

asset_impact_data <-
  asset_impact_data %>% dplyr::filter(asset_impact_data$company_id %in% unique(abcd_data$id))


# 4) run eikon data preparation-----
eikon_data <- prepare_eikon_data(
  eikon_data_input = eikon_data_input,
  security_financial_data = security_financial_data,
  consolidated_financial_data = consolidated_financial_data,
  ownership_tree = ownership_tree,
  asset_impact_data = asset_impact_data,
  country_region_bridge = country_region_bridge,
  n_min_sample = n_min_sample,
  min_ratio_sample_subgroup = min_ratio_sample_subgroup,
  range_profit_margin = range_profit_margin
)

## write results
eikon_data %>%
  readr::write_csv(fs::path(output_path_db_analysis_inputs, "eikon_financial_data.csv"))

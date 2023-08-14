devtools::load_all()

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


db_root_path <- r2dii.utils::path_dropbox_2dii(fs::path("PortCheck",
                                                        "00_Data",
                                                        "07_AnalysisInputs",
                                                        "db"))
abcd_path <- r2dii.utils::path_dropbox_2dii(
  "ST_INPUTS",
  "ST_INPUTS_MASTER",
  "abcd_stress_test_input.csv"
)
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



# 3) load input data----------------------------------------------------

abcd_data <- readr::read_csv(abcd_path)

# load data tables
# created by a merge of security_financial_data and consolidated_financial data
# by company_id.
# to this merge has been added additional data provided by Asset Impact in 2023Q2

# unique id: isin
ids <- readr::read_rds(fs::path(db_root_path, "db_ids.rds")) %>%
  assertr::verify(length(unique(.$isin)) == nrow(.))


# unique id: company_id
companies <-
  readr::read_rds(fs::path(db_root_path, "db_companies.rds")) %>%
  assertr::verify(length(unique(.$company_id)) == nrow(.))
#  TODO move to workflow branch
# companies <-
#   companies %>% dplyr::filter(companies$company_id %in% unique(abcd_data$id))

# unique id: isin
assets <- readr::read_rds(fs::path(db_root_path, "db_assets.rds"))%>%
  assertr::verify(length(unique(.$isin)) == nrow(.))

ownership_tree <- readr::read_rds(fs::path(db_root_path, "db_ownership_tree.rds"))
# prepare ownership tree----

ownership_tree <-
  prewrangle_ownership_tree(ownership_tree)

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


# TODO mismatch in sectors meaning between files
# companies %>%
#   inner_join(ids %>% distinct(company_id, isin)) %>%
#   inner_join(assets, by="isin") %>%
#   filter(security_mapped_sector!= "Other")  %>%
#   inner_join(company_activities  %>% distinct(id, ald_sector), by=c("company_id"="id")) %>%
#   distinct(ald_sector, bics_sector, bics_subgroup, security_mapped_sector)


# 4) run eikon data preparation-----
eikon_data <- prepare_eikon_data(
  ids=ids,
  companies=companies,
  assets=assets,
  ownership_tree = ownership_tree,
  country_region_bridge = country_region_bridge,
  n_min_sample = n_min_sample,
  min_ratio_sample_subgroup = min_ratio_sample_subgroup,
  range_profit_margin = range_profit_margin
)

# filter + assert ranges of indicators----

# TODO


# QA ----
# TODO: decide if eikon data prep QA graphs need to be kept, if so: move to
# adequate place and document idea
# data_source <- eikon_data %>%
#   dplyr::select(company_id, contains("overall_data_type")) %>%
#   tidyr::pivot_longer(cols = !company_id) %>%
#   dplyr::group_by(name, value) %>%
#   dplyr::summarise(n = n(), .groups = "drop") %>%
#   dplyr::ungroup() %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_col(ggplot2::aes(y = name, x = n, fill = value), position = "stack") +
#   viridis::scale_fill_viridis(discrete = TRUE)

# data_source_by_ultimate_parent <- eikon_data %>%
#   dplyr::select(company_id, is_ultimate_listed_parent, contains("overall_data_type")) %>%
#   tidyr::pivot_longer(cols = !c(company_id, is_ultimate_listed_parent)) %>%
#   dplyr::group_by(is_ultimate_listed_parent, name, value) %>%
#   dplyr::summarise(n = n(), .groups = "drop") %>%
#   dplyr::ungroup() %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_col(ggplot2::aes(y = name, x = n, fill = value), position = "stack") +
#   ggplot2::facet_wrap(~is_ultimate_listed_parent, scales = "free_x") +
#   viridis::scale_fill_viridis(discrete = TRUE)

# histogram_all_vars <- eikon_data %>%
#   dplyr::filter(is_ultimate_listed_parent == TRUE) %>%
#   dplyr::select(-contains(c("margin", "company"))) %>%
#   purrr::keep(is.numeric) %>%
#   tidyr::gather() %>%
#   ggplot2::ggplot(ggplot2::aes(value)) +
#   ggplot2::facet_wrap(~ key, scales = "free") +
#   ggplot2::geom_histogram()

# histogram_eikon_vars <- eikon_data %>%
#   dplyr::filter(is_ultimate_listed_parent == TRUE) %>%
#   dplyr::select(bloomberg_id, contains("margin"), -contains("type")) %>%
#   tidyr::pivot_longer(cols = !"bloomberg_id") %>%
#   dplyr::filter(dplyr::between(value, -10, 10)) %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_histogram(ggplot2::aes(x = value), alpha = 1, bins = 100, position = "identity") +
#   ggplot2::geom_vline(xintercept = 0) +
#   ggplot2::facet_wrap(~name, scales = "free_x")

# 5) save eikon data---------------------------------------------------
# optional - the file can be fairly large, so saving the file on the dropbox by
# default
eikon_data %>%
  readr::write_csv(fs::path(output_path_db_analysis_inputs, "eikon_financial_data.csv"))

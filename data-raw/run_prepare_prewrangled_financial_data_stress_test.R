devtools::load_all()

output_dir <- fs::path("data-raw", "DBs")

eikon_data <- readr::read_rds(fs::path(output_dir, "DB_assets_eikon.rds"))
companies_data <- readr::read_rds(fs::path(output_dir, "DB_asset_impact.rds"))
ids_data <- readr::read_rds(fs::path(output_dir, "DB_ids.rds"))
ownership_tree <- readr::read_rds(fs::path(output_dir, "DB_ownership_tree.rds"))


add_column_company_id_to_eikon_data <- function(eikon_data, ids_data) {
  isin_to_company_id <- ids_data %>% dplyr::distinct(.data$isin, .data$company_id)

  financial_data <- eikon_data %>%
    dplyr::inner_join(isin_to_company_id, by = c("isin"))

  return(financial_data)
}

financial_data <- add_column_company_id_to_eikon_data(eikon_data, ids_data)


# parameters for minimum requirements to reference subgroups in creating averages
# determine size of subgroup below which we do not use the average because the
# minimum required sample size of reference subgroup
minimum_sample_size <- 50
# minimum required ratio of reference subgroup to sample
minimum_ratio_sample <- 1 / 3
# cut off values for profit margins
allowed_range_npm <- c(-Inf, Inf)


prewrangled_financial_data_stress_test <- prepare_financial_data(
  financial_data = financial_data,
  companies_data = companies_data,
  ownership_tree = ownership_tree,
  minimum_sample_size = minimum_sample_size,
  minimum_ratio_sample = minimum_ratio_sample,
  allowed_range_npm = allowed_range_npm
)

abcd_data <- readr::read_csv(fs::path("data-raw", "abcd_stress_test_input.csv"))

prewrangled_financial_data_stress_test <- prewrangled_financial_data_stress_test %>%
  dplyr::inner_join(abcd_data %>% dplyr::distinct(company_id))


prewrangled_financial_data_stress_test %>% readr::write_csv(
  file.path("data-raw", "prewrangled_financial_data_stress_test.csv")
)

devtools::load_all()

rename_df_list_geographies <- function(df_list, geo_names_mapping) {
  rename_func <- purrr::partial(rename_scenario_geographies,
    geo_names_mapping = geo_names_mapping
  )
  df_list <- purrr::map(df_list, rename_func)
  return(df_list)
}

get_df_list_geographies <- function(df_list) {
  extracted_geographies <- purrr::map(df_list, function(df) {
    df$scenario_geography
  })
  all_geographies <- unique(unlist(extracted_geographies))
  all_geographies <- sort(all_geographies)
  return(all_geographies)
}

apply_geographies_renaming_fun <- function(df_list, renaming_fun) {
  all_geographies <- get_df_list_geographies(df_list)
  new_geographies_names <- renaming_fun(all_geographies)
  names(new_geographies_names) <- all_geographies
  df_list <- rename_df_list_geographies(df_list, new_geographies_names)
  return(df_list)
}

## load required data
prewrangled_capacity_factors <- readr::read_csv(here::here("data-raw", "prewrangled_capacity_factors.csv"))
price_data_long <- readr::read_csv(here::here("data-raw", "price_data_long.csv"))
scenarios_analysis_input <- readr::read_csv(here::here("data-raw", "Scenarios_AnalysisInput_2021.csv"))
bench_regions <- readr::read_csv(here::here("data-raw", "bench_regions.csv"))

# TODO use smallest common denominator to rename geographies ? i.e. all tables are joined in trisk anyway ?

## Check if all geographies exist in bench_regions
trisk_input_dfs <- list(
  prewrangled_capacity_factors,
  price_data_long,
  scenarios_analysis_input
)

all_geographies <- get_df_list_geographies(trisk_input_dfs)

stopifnot(all(all_geographies %in% bench_regions$scenario_geography))

## adds bench_regions to the list of dataframes to be renamed
# dfs_to_rename <- c(trisk_input_dfs, list(bench_regions))


### GENERIC RENAMING

## replace "&" character by and
to_and_fullword <- function(x) {
  stringr::str_replace_all(x, "&", " and ")
}
trisk_input_dfs <- apply_geographies_renaming_fun(trisk_input_dfs, to_and_fullword)
## capitalize first letter of each word, only where spaces exist
geography_to_title <- function(x) {
  ifelse(grepl(" ", x), stringr::str_to_title(x), x)
}
trisk_input_dfs <- apply_geographies_renaming_fun(trisk_input_dfs, geography_to_title)
## Remove all whitespaces in geographies names
no_whitespace_renaming <- function(x) {
  stringr::str_replace_all(x, " ", "")
}
trisk_input_dfs <- apply_geographies_renaming_fun(trisk_input_dfs, no_whitespace_renaming)
## Remove NGFS (R5) naming
no_r5_renaming <- function(x) {
  stringr::str_replace_all(x, "\\(R5\\)", "")
}
trisk_input_dfs <- apply_geographies_renaming_fun(trisk_input_dfs, no_r5_renaming)

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
bench_regions <- readr::read_csv(here::here("data-raw", "bench_regions.csv"))
prewrangled_capacity_factors <- readr::read_csv(here::here("data-raw", "prewrangled_capacity_factors.csv"))
price_data_long <- readr::read_csv(here::here("data-raw", "price_data_long.csv"))
scenarios_analysis_input <- readr::read_csv(here::here("data-raw", "Scenarios_AnalysisInput_2021.csv"))

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

### GROUP IDENTICAL GEOGRAPHIES
#' @param matching_tol percentage of country matching allowed to gropu geographies
group_identical_geographies <- function(bench_regions, matching_tol=1){
  # group country iso names into lists
  grouped_country_iso <- bench_regions %>%
    dplyr::group_by(scenario_geography) %>%
    dplyr::summarise(country_iso_list = list(country_iso))
  # create a dataframe with all geographies pairs
  match_country_iso <- dplyr::cross_join(grouped_country_iso, grouped_country_iso)
  # count how many country_iso are identical between geographies
  count_match_country_iso <- match_country_iso %>%
    dplyr::group_by(scenario_geography.x, scenario_geography.y) %>%
    dplyr::mutate(
      n_country_match = length(country_iso_list.x[country_iso_list.x %in% country_iso_list.y]) / length(country_iso_list.x)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(scenario_geography.x, scenario_geography.y, n_country_match)
  # keep geographies pairs having perfect matching
  identical_geographies <- count_match_country_iso %>%
    dplyr::filter(n_country_match >= matching_tol &
      scenario_geography.x != scenario_geography.y)
  # remove geographies pairs permutation duplicates
  identical_geographies <- identical_geographies[!duplicated(t(apply(identical_geographies, 1, sort))), ]

  # map each geography to the identical one having the longest name
  clean_identical <- identical_geographies %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      longest_name = dplyr::if_else(
        nchar(scenario_geography.x) >= nchar(scenario_geography.y),
        scenario_geography.x,
        scenario_geography.y
      ),
      shortest_name = dplyr::if_else(
        nchar(scenario_geography.x) >= nchar(scenario_geography.y),
        scenario_geography.y,
        scenario_geography.x
      )
    )
  clean_identical <- dplyr::anti_join(clean_identical, clean_identical,
    by = c("longest_name" = "shortest_name")
  )

  # create renaming mapper. Use longest name as new geography name
  mapper <- clean_identical$longest_name
  names(mapper) <- clean_identical$shortest_name
  return(mapper)
}
mapper <- group_identical_geographies(bench_regions)
trisk_input_dfs <- rename_df_list_geographies(trisk_input_dfs, mapper)

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

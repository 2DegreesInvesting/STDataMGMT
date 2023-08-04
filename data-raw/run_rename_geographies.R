devtools::load_all()

## load required data
bench_regions <-
  readr::read_csv(here::here("data-raw", "bench_regions.csv"), na = c(""))
bench_regions <- bench_regions %>%
  dplyr::mutate(scenario_geography_newname = scenario_geography)

trisk_input_dfs_paths <- c(
  here::here("data-raw", "prewrangled_capacity_factors.csv"),
  here::here("data-raw", "price_data_long.csv"),
  here::here("data-raw", "Scenarios_AnalysisInput_2021.csv")
)
trisk_input_dfs <- lapply(trisk_input_dfs_paths, readr::read_csv)
names(trisk_input_dfs) <- trisk_input_dfs_paths


### COHERENCE CHECK
get_df_list_geographies <- function(df_list) {
  extracted_geographies <- purrr::map(df_list, function(df) {
    df$scenario_geography
  })
  all_geographies <- unique(unlist(extracted_geographies))
  all_geographies <- sort(all_geographies)
  return(all_geographies)
}
## Check if all geographies exist in bench_regions
all_geographies <- get_df_list_geographies(trisk_input_dfs)
stopifnot(all(all_geographies %in% bench_regions$scenario_geography))


### GROUP IDENTICAL GEOGRAPHIES
#' @param matching_tol percentage of country matching allowed to gropu geographies
group_identical_geographies <-
  function(bench_regions, matching_tol = 1) {
    # group country iso names into lists
    grouped_country_iso <- bench_regions %>%
      dplyr::group_by(scenario_geography) %>%
      dplyr::summarise(country_iso_list = list(country_iso))
    # create a dataframe with all geographies pairs
    match_country_iso <-
      dplyr::cross_join(grouped_country_iso, grouped_country_iso)
    # count how many country_iso are identical between geographies
    count_match_country_iso <- match_country_iso %>%
      dplyr::group_by(scenario_geography.x, scenario_geography.y) %>%
      dplyr::mutate(n_country_match = length(country_iso_list.x[country_iso_list.x %in% country_iso_list.y]) / length(country_iso_list.x)) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        scenario_geography.x,
        scenario_geography.y,
        n_country_match
      )
    # keep geographies pairs having perfect matching
    identical_geographies <- count_match_country_iso %>%
      dplyr::filter(n_country_match >= matching_tol &
        scenario_geography.x != scenario_geography.y)
    # remove geographies pairs permutation duplicates
    identical_geographies <-
      identical_geographies[!duplicated(t(apply(identical_geographies, 1, sort))), ]

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
    clean_identical <-
      dplyr::anti_join(clean_identical,
        clean_identical,
        by = c("longest_name" = "shortest_name")
      )

    # create renaming mapper. Use longest name as new geography name
    geo_group_mapper <- clean_identical$longest_name
    names(geo_group_mapper) <- clean_identical$shortest_name
    return(geo_group_mapper)
  }
geo_group_mapper <- group_identical_geographies(bench_regions)
bench_regions <-
  rename_column_values(
    bench_regions,
    "scenario_geography_newname",
    geo_group_mapper
  )

### GENERIC RENAMING

#' apply a function to rename a vector of characters, and apply the mapping on the original column
rename_bench_region_geographies <-
  function(bench_regions, renaming_fun) {
    old_names <- unique(bench_regions$scenario_geography_newname)
    new_names <- renaming_fun(old_names)
    rename_mapping <- setNames(new_names, old_names)
    bench_regions <- rename_column_values(
      bench_regions,
      "scenario_geography_newname",
      rename_mapping
    )
    return(bench_regions)
  }
## replace "&" character by and
to_and_fullword <- function(x) {
  stringr::str_replace_all(x, "&", " and ")
}
bench_regions <-
  rename_bench_region_geographies(bench_regions, to_and_fullword)
## capitalize first letter of each word, only where spaces exist
geography_to_title <- function(x) {
  ifelse(grepl(" ", x), stringr::str_to_title(x), x)
}
bench_regions <-
  rename_bench_region_geographies(bench_regions, geography_to_title)
## Remove all whitespaces in geographies names
no_whitespace_renaming <- function(x) {
  stringr::str_replace_all(x, " ", "")
}
bench_regions <-
  rename_bench_region_geographies(bench_regions, no_whitespace_renaming)
## Remove NGFS (R5) naming
no_r5_renaming <- function(x) {
  stringr::str_replace_all(x, "\\(R5\\)", "")
}
bench_regions <-
  rename_bench_region_geographies(bench_regions, no_r5_renaming)

bench_regions %>% readr::write_csv(here::here("data-raw", "bench_regions.csv"), na = c(""))

### TRISK INPUTS RENAMING

bench_regions_unique_geographies <- bench_regions %>%
  dplyr::distinct(scenario_geography, scenario_geography_newname)
geographies_old_names <-
  bench_regions_unique_geographies$scenario_geography
geographies_new_names <-
  bench_regions_unique_geographies$scenario_geography_newname
final_geo_renaming <-
  setNames(geographies_new_names, geographies_old_names)

trisk_input_dfs <- purrr::map(
  trisk_input_dfs,
  purrr::partial(
    rename_column_values,
    colname = "scenario_geography",
    key_value_mapping = final_geo_renaming
  )
)

for (fp in names(trisk_input_dfs)) {
  readr::write_csv(trisk_input_dfs[[fp]], fp)
}

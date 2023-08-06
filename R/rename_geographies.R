#' Title
#'
#' @param path_prewrangled_capacity_factors path_prewrangled_capacity_factors
#' @param path_price_data_long path_price_data_long
#' @param path_Scenarios_AnalysisInput path_Scenarios_AnalysisInput
#'
#' @return trisk_input_dfs
load_trisk_inputs <-
  function(path_prewrangled_capacity_factors,
           path_price_data_long,
           path_Scenarios_AnalysisInput) {
    {
      trisk_input_dfs_paths <- c(
        path_prewrangled_capacity_factors,
        path_price_data_long,
        path_Scenarios_AnalysisInput
      )

      trisk_input_dfs <-
        lapply(trisk_input_dfs_paths, readr::read_csv)
      names(trisk_input_dfs) <- trisk_input_dfs_paths
    }

    return(trisk_input_dfs)
  }


#' Title
#'
#' @param trisk_input_dfs trisk_input_dfs
#'
#' @return all unique geographies accross input dfs
get_all_unique_geographies <- function(trisk_input_dfs) {
  extracted_geographies <- purrr::map(trisk_input_dfs, function(df) {
    df$scenario_geography
  })
  all_geographies <- unique(unlist(extracted_geographies))
  all_geographies <- sort(all_geographies)
  return(all_geographies)
}



#' GROUP IDENTICAL GEOGRAPHIES
#' @param bench_regions bench_regions
#'
#' @param matching_tol percentage of country matching allowed to group 2 geographies
group_identical_geographies <-
  function(bench_regions, matching_tol) {
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

#' apply a function to rename a vector of characters, and apply the mapping on the original column
#'
#' @param bench_regions bench_regions
#' @param renaming_fun a renaming function that takes a single string as parameter
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

#' Title
#'
#' @param bench_regions bench_regions
#' @param list_rules list of functions each defining a string renaming rule
#'
#' @return bench_regions
apply_geographies_renaming_rules <-
  function(bench_regions, list_rules) {
    for (rule_fun in list_rules) {
      bench_regions <-
        rename_bench_region_geographies(bench_regions, rule_fun)
    }
    return(bench_regions)
  }

### GENERIC RENAMING

#' Title
#'
#' @param bench_regions bench_regions
#'
#' @return bench_regions
standardize_geoographies_name <- function(bench_regions) {
  ## replace "&" character by and
  to_and_fullword <- function(x) {
    stringr::str_replace_all(x, "&", " and ")
  }
  ## capitalize first letter of each word, only where spaces exist
  geography_to_title <- function(x) {
    ifelse(grepl(" ", x), stringr::str_to_title(x), x)
  }
  ## Remove all whitespaces in geographies names
  no_whitespace_renaming <- function(x) {
    stringr::str_replace_all(x, " ", "")
  }
  ## Remove NGFS (R5) naming
  no_r5_renaming <- function(x) {
    stringr::str_replace_all(x, "\\(R5\\)", "")
  }
  list_rules <-
    list(
      to_and_fullword,
      geography_to_title,
      no_whitespace_renaming,
      no_r5_renaming
    )

  bench_regions <-
    apply_geographies_renaming_rules(bench_regions, list_rules)
  return(bench_regions)
}



### TRISK INPUTS RENAMING
#' Title
#'
#' @param bench_regions bench_regions
#' @param trisk_input_dfs trisk_input_dfs
#'
#' @return trisk_input_dfs
rename_stress_test_inputs <-
  function(bench_regions, trisk_input_dfs) {
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
    return(trisk_input_dfs)
  }


#' Title
#'
#' @param bench_regions bench_regions
#' @param path_prewrangled_capacity_factors path_prewrangled_capacity_factors
#' @param path_price_data_long path_price_data_long
#' @param path_Scenarios_AnalysisInput path_Scenarios_AnalysisInput
#' @param matching_tol  percentage of country matching allowed to group 2 geographies
#'
#' @return list containing bench_regions and trisk_input_dfs
#' @export
#'
regroup_and_rename_geographies <-
  function(bench_regions,
           path_prewrangled_capacity_factors,
           path_price_data_long,
           path_Scenarios_AnalysisInput,
           matching_tol = 1) {
    trisk_input_dfs <-
      load_trisk_inputs(
        path_prewrangled_capacity_factors,
        path_price_data_long,
        path_Scenarios_AnalysisInput
      )

    ## Check if all geographies exist in bench_regions
    # all_geographies <- get_all_unique_geographies(trisk_input_dfs)
    # stopifnot(all(all_geographies %in% bench_regions$scenario_geography))

    geo_group_mapper <-
      group_identical_geographies(bench_regions, matching_tol = matching_tol)
    bench_regions <- bench_regions %>%
      dplyr::mutate(scenario_geography_newname = scenario_geography) %>%
      rename_column_values(
        "scenario_geography_newname",
        geo_group_mapper
      )

    bench_regions <- standardize_geoographies_name(bench_regions)

    trisk_input_dfs <-
      rename_stress_test_inputs(bench_regions, trisk_input_dfs)

    return(list(trisk_input_dfs = trisk_input_dfs, bench_regions = bench_regions))
  }

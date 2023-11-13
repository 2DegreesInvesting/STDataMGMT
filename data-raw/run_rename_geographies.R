devtools::load_all()
library(dplyr)


cleaning_bench_regions <- function(bench_regions_input_path) {
  bench_regions <- read.csv(bench_regions_input_path, fill = TRUE)
  bench_regions <- tibble::as_tibble(bench_regions)

  country_to_iso2c <- countrycode::codelist %>%
    filter(!is.na(ecb), !is.na(`country.name.en`)) %>%
    distinct(ecb, `country.name.en`) %>%
    rename(country = `country.name.en`, country_iso = ecb)

  # remove countries with countrycode EU, unknown as a country (is European but which one ?)
  # also contains only 1 country (russia)
  bench_regions <- bench_regions %>% filter(country_iso != "EU")
  # ADD REGIONS ===============================================================

  # Adds new geographies
  new_geos <- readxl::read_excel("data-raw/matchingregions.xlsx") %>%
    bind_rows(tibble::tribble(~scenario_geography, ~country, c("Coastal China"), c("China")))

  new_geos_in_countrycodes <- new_geos %>%
    dplyr::inner_join(country_to_iso2c,
      by = "country"
    )
  new_geos_not_in_countrycodes <- new_geos %>%
    dplyr::anti_join(country_to_iso2c,
      by = "country"
    )

  # All commented countries (30) belong to the Developing Economies geography
  remap_countries <-
    c(
      "Myanmar" = "MM",
      "Plurinational State of Bolivia" = "BO",
      "Curaçao/Netherlands Antilles" = "CW",
      "Trinidad and Tobago" = "TT",
      "Bolivarian Republic of Venezuela" = "VE",
      "Antigua And Barbuda" = "AG",
      "Bosnia and Herzegovina" = "BA",
      "Brunei Darussalam" = "BN",
      "Curacao" = "CW",
      "Democratic People's Republic of Korea" = "KP",
      "Democratic Republic of the Congo" = "CD",
      "GuineaBissau" = "GW",
      "Hong Kong Special Administrative Region of China" = "HK",
      "Iran (Islamic Republic of Iran)" = "IR",
      "Islamic Republic of Mauritania" = "MR",
      "Ivory Coast" = "CI",
      "Lao People's Democratic Republic" = "LA",
      "Libyan Arab Jamahiriya" = "LY",
      "Macau Special Administrative Region of China" = "MO",
      "Republic of Congo" = "CG",
      "Republic of Korea" = "KP",
      # is in developping countries
      "Republic of Macedonia" = "MK",
      "Republic of Moldova" = "MD",
      "Reunion" = "RE",
      "Russian Federation" = "RU",
      # "Somaliland",
      # "Swaziland",
      "Syrian Arab Republic" = "SY",
      "TimorLeste" = "TL",
      "United Republic Of Tanzania" = "TZ",
      # "Virgin Islands (US)",
      # "AngolaCongo Republic Joint Development Area",
      "Congo Republic" = "CG",
      # "Cote d'Ivoire",
      "KuwaitSaudi Arabia Partitioned Neutral Zone" = "KW",
      # "Malaysia  Thailand Joint Development Area",
      # "Malaysia  Vietnam PM3 Commercial Arrangement Area",
      # "Timor Sea Joint Petroleum Development Area",
      # "Saint Barthelemy",
      # "Saint Helena,  Ascension and Tristan da Cunha",
      # "Saint Kitts and Nevis",
      # "Saint Lucia",
      # "Saint Pierre and Miquelon",
      # "Saint Vincent and the Grenadines",
      # "Bonaire",
      "Congo" = "CG",
      # "Falkland Islands (Malvinas)",
      # "Heard Island and McDonald Mcdonald Islands",
      # "Holy See (Vatican City State)",
      "Iran,  Islamic Republic of" = "IR",
      "Korea,  Democratic People's Republic of" = "KP",
      "Macedonia, the Former Yugoslav Republic of" = "MK",
      # "Micronesia, Federated States of",
      "Moldova, Republic of" = "MD",
      "Palestine, State of" = "PS",
      # "Pitcairn",
      # "Saint Helena",
      # "Sao Tome and Principe",
      # "Sint Maarten (Dutch part)",
      # "South Georgia and the South Sandwich Islands",
      # "Svalbard and Jan Mayen",
      "Taiwan, Province of China" = "TW",
      # "Turks and Caicos Islands",
      # "United States Minor Outlying Islands",
      "Viet Nam" = "VN",
      # "US Virgin Islands",
      "Wallis and Futuna" = "WF"
      # "Aland Islands"
    )

  new_geos_added_countrycodes <- new_geos_not_in_countrycodes %>%
    filter(country %in% names(remap_countries)) %>%
    mutate(country_iso = remap_countries[country])

  new_geos_and_countrycodes <-
    bind_rows(new_geos_in_countrycodes, new_geos_added_countrycodes)
  new_geos_and_countrycodes <- new_geos_and_countrycodes %>%
    group_by(scenario_geography) %>%
    mutate(reg_count = n()) %>%
    ungroup()

  bench_regions <- bind_rows(bench_regions, new_geos_and_countrycodes)

  # REMOVE DUPLICATES ===============================================================

  # rewrite the is code for namidia as NA
  bench_regions <-
    bench_regions %>% mutate(country_iso = if_else(is.na(country_iso), "NA", country_iso))

  # clean duplicates countries in bench_regions Global geographies
  # (actually not a problem, since there is a distinct(scenario_geography, country_iso) in the prod script:
  #     || otherwise those country's production would be counted twice or more in ABCD
  # 7 geographies concerned. To check :
  duplicate_regions <- bench_regions %>%
    group_by(scenario_geography) %>%
    summarise(n_rows = n(), n_unique_iso2c = length(unique(country_iso))) %>%
    filter(n_rows != n_unique_iso2c) %>%
    pull(scenario_geography)

  # remove duplicated
  bench_regions_no_dupl <- bench_regions %>%
    filter(!scenario_geography %in% duplicate_regions)
  bench_regions_dupl <- bench_regions %>%
    filter(scenario_geography %in% duplicate_regions)

  bench_regions_cleaned_dupl <- bench_regions_dupl %>%
    group_by(scenario_geography, country_iso) %>%
    filter(row_number() == 1)

  bench_regions <-
    bind_rows(bench_regions_no_dupl, bench_regions_cleaned_dupl)

  bench_regions <-
    bench_regions %>%
    group_by(scenario_geography) %>%
    mutate(reg_count = n()) %>%
    ungroup()


  # RENAME COUNTRY ===============================================================

  # remove all country names and replace them by the column `country.name.en`
  # of `countrycode::codelist`
  # using the iso2c as join key
  # 32 countrycodes  concerned. Proof:
  sum(
    bench_regions %>% distinct(country, country_iso) %>% group_by(country_iso) %>% summarise(nrow = n()) %>% arrange(desc(nrow)) %>% print(n = 100) %>% pull(nrow) > 1
  )
  # rename countries
  bench_regions <- bench_regions %>%
    select(c(-country)) %>%
    dplyr::left_join(country_to_iso2c,
      by = c("country_iso")
    )

  return(bench_regions)
}



bench_regions_input_path <- here::here("data-raw", "bench_regions.csv")
bench_regions <- cleaning_bench_regions(bench_regions_input_path)

path_prewrangled_capacity_factors <-
  here::here("data-raw", "prewrangled_capacity_factors.csv")
path_price_data_long <-
  here::here("data-raw", "price_data_long.csv")
path_Scenarios_AnalysisInput <-
  here::here("data-raw", "Scenarios_AnalysisInput_2021.csv")

matching_tol <- 1

output_list <- regroup_and_rename_geographies(
    bench_regions = bench_regions,
    path_prewrangled_capacity_factors = path_prewrangled_capacity_factors,
    path_price_data_long = path_price_data_long,
    path_Scenarios_AnalysisInput = path_Scenarios_AnalysisInput,
    matching_tol = matching_tol
  )

  scenarios_geographies <- output_list[["bench_regions"]]
  trisk_input_dfs <- output_list[["trisk_input_dfs"]]

  ## ------------------------------ WRITE OUTPUTS
  usethis::use_data(scenarios_geographies, overwrite = T)

  for (fp in names(trisk_input_dfs)) {
    readr::write_csv(trisk_input_dfs[[fp]], fp)
  }




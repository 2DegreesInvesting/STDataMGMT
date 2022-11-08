devtools::load_all()
library(pointblank)
library(dplyr)

input_path_price_data_long <- path_dropbox_2dii(
  "ST_INPUTS",
  "ST_INPUTS_MASTER",
  "price_data_long.csv"
)

price_data_long_input <- readr::read_csv(
  file.path(input_path_price_data_long)
)


informant_pp <-
  create_informant(
    tbl = price_data_long_input,
    tbl_name = "price_data_long",
    label = "Price data from **WEOYYYY_fossil_fuel_prices_by_scenario.csv** (fossil fuels)
    and **WEOYYYY_power_generation_technology_costs.csv** (power sector) 📦."
  ) %>%
  info_columns(
    columns = "year",
    `ℹ️` = "Year in the range between {min_year} to {max_year}."
  ) %>%
  info_snippet(
    snippet_name = "min_year",
    fn = snip_lowest(column = "year")
  ) %>%
  info_snippet(
    snippet_name = "max_year",
    fn = snip_highest(column = "year")
  ) %>%
  info_columns(
    columns = "scenario",
    `ℹ️` = "Climate change scenarios: {scenario_snippet}."
  ) %>%
  info_snippet(
    snippet_name = "scenario_snippet",
    fn = snip_list(column = "scenario", limit = 10)
  ) %>%
  info_columns(
    columns = "scenario_geography",
    `ℹ️` = "Name of the {scenario_geography_count} global regions under analysis: {scenario_geography_snippet}."
  ) %>%
  info_snippet(
    snippet_name = "scenario_geography_snippet",
    fn = snip_list(column = "scenario_geography", limit = 10)
  ) %>%
  info_snippet(
    snippet_name = "scenario_geography_count",
    fn = ~ . %>%
      .$scenario_geography %>%
      n_distinct()
  ) %>%
  info_columns(
    columns = "sector",
    `ℹ️` = "Emission emitting industry under analysis {sector_snippet}."
  ) %>%
  info_snippet(
    snippet_name = "sector_snippet",
    fn = snip_list(column = "sector")
  ) %>%
  info_columns(
    columns = "technology",
    `ℹ️` = "Name of the energy producing technology {technology_snippet}."
  ) %>%
  info_snippet(
    snippet_name = "technology_snippet",
    fn = snip_list(column = "technology", limit = 10)
  ) %>%
  info_columns(
    columns = "indicator",
    `ℹ️` = "Price {indicator_snippet}."
  ) %>%
  info_snippet(
    snippet_name = "indicator_snippet",
    fn = snip_list(column = "indicator")
  ) %>%
  info_columns(
    columns = "unit",
    `ℹ️` = "Unit for price based on technology: {unit_snippet}."
  ) %>%
  info_snippet(
    snippet_name = "unit_snippet",
    fn = snip_list(column = "unit")
  ) %>%
  info_columns(
    columns = "price",
    `ℹ️` = "Floating price, with an average of {price_mean}.",
    `Stats` = "(fivenum): {price_summary}"
  ) %>%
  info_snippet(
    snippet_name = "price_summary",
    fn = snip_stats(column = "price")
  ) %>%
  info_snippet(
    snippet_name = "price_mean",
    fn = ~ . %>%
      .$price %>%
      mean(na.rm = TRUE) %>%
      round(2)
  ) %>%
  info_tabular(
    `Dataset description` = "`price_data_long.csv` provides information on prices of fossil fuels and for power generation under various WEO scenarios. Power generation prices are captured by the levelized cost of electricity.",
    `Raw files used` = "Input files are stored on dropbox under PortCheck/00_Data/01_ProcessedData/03_ScenarioData/RawData and
    titled `WEOYYYY_fossil_fuel_prices_by_scenario.csv` and
    `WEOYYYY_power_generation_technology_costs.csv`, respectivley."
  ) %>%
  incorporate()

get_informant_report(informant_pp, title = "Data Dictionary") |> export_report(filename = affix_datetime
("./test_investigate_data_sets/dictionary_price_data_long.html"))

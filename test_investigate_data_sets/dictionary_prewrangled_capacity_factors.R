devtools::load_all()
library(pointblank)
library(dplyr)

input_path_prewrangled_capacity_factor <- path_dropbox_2dii(
  "ST_INPUTS",
  "ST_INPUTS_MASTER",
  "prewrangled_capacity_factors.csv"
)

prewrangled_capacity_factor_input <- readr::read_csv(
  file.path(input_path_prewrangled_capacity_factor)
)


informant_pp <-
  create_informant(
    tbl = prewrangled_capacity_factor_input,
    tbl_name = "prewrangled_capacity_factor",
    label = "Capacity factors in the power sector from **WEOYYYY_Raw_Data.csv** 📦."
  ) %>%
  info_columns(
    columns = "scenario",
    `ℹ️` = "Climate change scenarios: {scenario_snippet}."
  ) %>%
  info_snippet(
    snippet_name = "scenario_snippet",
    fn = snip_list(column = "scenario")
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
    columns = "scenario_geography",
    `ℹ️` = "Name of the {scenario_geography_count} global regions under analysis ({scenario_geography_snippet})."
  ) %>%
  info_snippet(
    snippet_name = "scenario_geography_snippet",
    fn = snip_list(column = "scenario_geography")
  ) %>%
  info_snippet(
    snippet_name = "scenario_geography_count",
    fn = ~ . %>%
      .$scenario_geography %>%
      n_distinct()
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
    columns = "capacity_factor",
    `ℹ️` = "Share of total power capacity which is in use (and producing energy).",
    `Stats` = "(fivenum): {capacity_factor_summary}"
  ) %>%
  info_snippet(
    snippet_name = "capacity_factor_summary",
    fn = snip_stats(column = "capacity_factor")
  ) %>%
  info_tabular(
    `Dataset description` = "`prewrangled_capacity_factors.csv` provides information on the capacity factors in the power sector that are used in the stress test.",
    `Raw files used` = "Input files are stored on dropbox under PortCheck/00_Data/01_ProcessedData/03_ScenarioData/RawData and titled `WEOYYYY_Raw_Data.csv`, with YYYY being the year of the release of the report."
  ) %>%
  incorporate()


get_informant_report(informant_pp, title = "Data Dictionary") |> export_report(filename = affix_datetime
("./test_investigate_data_sets/dictionary_prewrangled_capacity_factors.html"))

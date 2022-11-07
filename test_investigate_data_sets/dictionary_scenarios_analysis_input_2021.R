devtools::load_all()
library(pointblank)
library(dplyr)


input_path_scenarios_analysis_input <- path_dropbox_2dii(
  "ST_INPUTS",
  "ST_INPUTS_MASTER",
  "Scenarios_AnalysisInput_2021.csv"
)

scenarios_analysis_input <- readr::read_csv(
  file.path(input_path_scenarios_analysis_input)
)


informant_pp <-
  create_informant(
    tbl = scenarios_analysis_input,
    tbl_name = "Scenarios Analysis Input 2021",
    label = "Scenario analysis input from **Scenarios_AnalysisInput_2021.csv** PACTA prewrangled 📦."
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
    `ℹ️` = "Name of energy producing technology {technology_snippet}."
  ) %>%
  info_snippet(
    snippet_name = "technology_snippet",
    fn = snip_list(column = "technology", limit = 15)
  ) %>%
  info_columns(
    columns = "ald_sector",
    `ℹ️` = "Emission emitting industries under analysis {ald_sector_snippet}."
  ) %>%
  info_snippet(
    snippet_name = "ald_sector_snippet",
    fn = snip_list(column = "ald_sector")
  ) %>%
  info_columns(
    columns = "scenario",
    `ℹ️` = "{scenario_count} climate change scenarios: {scenario_snippet}."
  ) %>%
  info_snippet(
    snippet_name = "scenario_snippet",
    fn = snip_list(column = "scenario", limit = 15)
  ) %>%
  info_snippet(
    snippet_name = "scenario_count",
    fn = ~ . %>%
      .$scenario %>%
      n_distinct()
  ) %>%
  info_columns(
    columns = "units",
    `ℹ️` = "Unit of measurement for energy production {units_snippet}."
  ) %>%
  info_snippet(
    snippet_name = "units_snippet",
    fn = snip_list(column = "units", limit = 7)
  ) %>%
  info_columns(
    columns = "direction",
    `ℹ️` = "Direction of the technology trend: {direction_snippet}."
  ) %>%
  info_snippet(
    snippet_name = "direction_snippet",
    fn = snip_list(column = "direction")
  ) %>%
  info_columns(
    columns = "fair_share_perc",
    `ℹ️` = "Target rate applied to production.",
    `Stats` = "(fivenum): {fair_share_perc_summary}"
  ) %>%
  info_snippet(
    snippet_name = "fair_share_perc_summary",
    fn = snip_stats(column = "fair_share_perc")
  ) %>%
  info_tabular(
    `Dataset description` = "`Scenarios_AnalysisInput_2021.csv` provides the input of different climate change scenarios for the stress test.",
    `Raw files used` = "Input file is stored on dropbox under PortCheck/00_Data/01_ProcessedData/03_ScenarioData and titled `Scenarios_AnalysisInput_2021.csv`."
  ) %>%
  incorporate()


get_informant_report(informant_pp, title = "Data Dictionary") |> export_report(filename = affix_datetime
("./test_investigate_data_sets/dictionary_scenarios_analysis_input_2021.html"))

devtools::load_all()
library(pointblank)
library(dplyr)

input_path_abcd_stress_test_input <- path_dropbox_2dii(
  "ST_INPUTS",
  "ST_INPUTS_MASTER",
  "abcd_stress_test_input.csv"
)

abcd_stress_test_input <- readr::read_csv(
  file.path(input_path_abcd_stress_test_input)
)


informant_pp <-
  create_informant(
    tbl = abcd_stress_test_input,
    tbl_name = "abcd_stress_test_input",
    label = "Production inputs from  **PAMS data set** 📦."
  ) %>%
  info_columns(
    columns = "id",
    `ℹ️` = "Unique company number, based on an Asset Resolution classification, between
           {min_id_snippet} and {max_id_snippet}."
  ) %>%
  info_snippet(
    snippet_name = "min_id_snippet",
    fn = snip_lowest(column = "id")
  ) %>%
  info_snippet(
    snippet_name = "max_id_snippet",
    fn = snip_highest(column = "id")
  ) %>%
  info_columns(
    columns = "company_name",
    `ℹ️` = "Name of the {company_name_snippet} companies, to which the physical assets are tied to."
  ) %>%
  info_snippet(
    snippet_name = "company_name_snippet",
    fn = ~ . %>%
      .$company_name %>%
      n_distinct()
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
    columns = "ald_sector",
    `ℹ️` = "Emission emitting industries under analysis {ald_sector_snippet}."
  ) %>%
  info_snippet(
    snippet_name = "ald_sector_snippet",
    fn = snip_list(column = "ald_sector")
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
    columns = "plan_tech_prod",
    `ℹ️` = "5-year-ahead forward-looking production for this company/technology. On average, {mean_plan_tech_prod} MW.",
    `Stats` = "(fivenum): {plan_tech_prod_summary}"
  ) %>%
  info_snippet(
    snippet_name = "mean_plan_tech_prod",
    fn = ~ . %>%
      .$plan_tech_prod %>%
      mean(na.rm = TRUE) %>%
      round(2)
  ) %>%
  info_snippet(
    snippet_name = "plan_tech_prod_summary",
    fn = snip_stats(column = "plan_tech_prod")
  ) %>%
  info_columns(
    columns = "plan_emission_factor",
    `ℹ️` = "Emission rate of the asset or the company's assets (directly and indirectly owned), relative to units of technology activity.
             Ranges between {min_plan_emission_factor} and {max_plan_emission_factor}.",
    `Stats` = "(fivenum): {plan_emission_factor_summary}"
  ) %>%
  info_snippet(
    snippet_name = "min_plan_emission_factor",
    fn = snip_lowest(column = "plan_emission_factor")
  ) %>%
  info_snippet(
    snippet_name = "max_plan_emission_factor",
    fn = snip_highest(column = "plan_emission_factor")
  ) %>%
  info_snippet(
    snippet_name = "plan_emission_factor_summary",
    fn = snip_stats(column = "plan_emission_factor")
  ) %>%
  info_columns(
    columns = plan_sec_prod,
    `ℹ️` = "5-year-ahead forward-looking production for the whole sector (can be multiple companies/technologies aggregated). On average, {mean_plan_sec_prod} MW.",
    `Stats` = "(fivenum): {plan_sec_prod_summary} "
  ) %>%
  info_snippet(
    snippet_name = "plan_sec_prod_summary",
    fn = snip_stats(column = "plan_sec_prod")
  ) %>%
  info_snippet(
    snippet_name = "mean_plan_sec_prod",
    fn = ~ . %>%
      .$plan_sec_prod %>%
      mean(na.rm = TRUE) %>%
      round(2)
  ) %>%
  info_tabular(
    `Dataset description` = "`abcd_stress_test_input.csv` provides production data for the stress test.",
    `Raw files used` = "Data were collected and made available by Asset Resolution. Input file is stored
     under ST_INPUTS/ST_INPUTS_PRODUCTION and titled `2022-02-17_AR_2021Q4_2DII-PAMS-Data.xlsx`."
  ) %>%
  incorporate()


get_informant_report(informant_pp, title = "Data Dictionary") |> export_report(filename = affix_datetime
("./test_investigate_data_sets/dictionary_abcd_stress_test_input.html"))

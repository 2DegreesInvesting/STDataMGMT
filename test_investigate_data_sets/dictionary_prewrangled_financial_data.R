devtools::load_all()
library(pointblank)
library(dplyr)

input_path_prewrangled_financial_data_stress_test <- path_dropbox_2dii(
  "ST_INPUTS",
  "ST_INPUTS_MASTER",
  "prewrangled_financial_data_stress_test.csv"
)

prewrangled_financial_data_stress_test_input <- readr::read_csv(
  file.path(input_path_prewrangled_financial_data_stress_test)
)


informant_pp <-
  create_informant(
    tbl = prewrangled_financial_data_stress_test_input,
    tbl_name = "prewrangled_financial_data_stress_test",
    label = "Financial data 📦."
  ) %>%
  info_columns(
    columns = "company_id",
    `ℹ️` = "Unique company number, based on an Asset Resolution classification, between
           {min_company_id_snippet} and {max_company_id_snippet}."
  ) %>%
  info_snippet(
    snippet_name = "min_company_id_snippet",
    fn = snip_lowest(column = "company_id")
  ) %>%
  info_snippet(
    snippet_name = "max_company_id_snippet",
    fn = snip_highest(column = "company_id")
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
    columns = "corporate_bond_ticker",
    `ℹ️` = "{corporate_bond_ticker_snippet} bond identifiers."
  ) %>%
  info_snippet(
    snippet_name = "corporate_bond_ticker_snippet",
    fn = ~ . %>%
      .$corporate_bond_ticker %>%
      n_distinct()
  ) %>%
  info_columns(
    columns = "pd",
    `ℹ️` = "Probability of default.",
    `Stats` = "(fivenum): {pd_summary}"
  ) %>%
  info_snippet(
    snippet_name = "pd_summary",
    fn = snip_stats(column = "pd")
  ) %>%
  info_columns(
    columns = "net_profit_margin",
    `Stats` = "(fivenum): {net_profit_margin_summary}"
  ) %>%
  info_snippet(
    snippet_name = "net_profit_margin_summary",
    fn = snip_stats(column = "net_profit_margin")
  ) %>%
  info_columns(
    columns = "debt_equity_ratio",
    `ℹ️` = "Ratio of outstanding debt to the company's equity value, on average {debt_equity_ratio_mean}.",
    `Stats` = "(fivenum): {debt_equity_ratio_summary}"
  ) %>%
  info_snippet(
    snippet_name = "debt_equity_ratio_summary",
    fn = snip_stats(column = "debt_equity_ratio")
  ) %>%
  info_snippet(
    snippet_name = "debt_equity_ratio_mean",
    fn = ~ . %>%
      .$debt_equity_ratio %>%
      mean(na.rm = TRUE) %>%
      round(2)
  ) %>%
  info_columns(
    columns = "volatility",
    `ℹ️` = "Asset volatility between {min_volatility} and {max_volatility}."
  ) %>%
  info_snippet(
    snippet_name = "max_volatility",
    fn = snip_highest(column = "volatility")
  ) %>%
  info_snippet(
    snippet_name = "min_volatility",
    fn = snip_lowest(column = "volatility")
  ) %>%
  info_tabular(
    `Dataset description` = "`prewrangled_financial_data_stress_test.csv` provides company-level financial information. Financial data from Eikon are merged with master data files provided by Asset Resolution.",
    `Raw files used` =
      "- Eikon input data files are stored on dropbox under PortCheck/00_Data/01_ProcessedData/02_FinancialData/Eikon_Data/2021Q2.

     - `Security_financial_data.rda`, `consolidated_financial_data.rda`, `masterdata_ownership_datastore.rda`, `masterdata_debt_datastore.rda` are stored on dropbox under PortCheck/00_Data/07_AnalysisInputs.

     - `company_ownership_bidirectional.csv` and `masterdata_credit_methodology.csv` are stored on dropox under PortCheck/00_Data/06_DataStore/DataStore_export_05172021/2020Q4."
  ) %>%
  incorporate()

get_informant_report(informant_pp, title = "Data Dictionary") |> export_report(filename = affix_datetime
("./test_investigate_data_sets/dictionary_prewrangled_financial_data_stress_test.html"))

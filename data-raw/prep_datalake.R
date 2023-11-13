devtools::load_all()
library(dplyr)




make_eikon_db <- function() {
  get_old_eikon_data <- function() {
    # path_db_eikon_data <- fs::path(
    #   r2dii.utils::dbox_port_00(), "02_FinancialData", "Eikon_Data", "2021Q2"
    # )
    path_db_eikon_data <- fs::path("data-raw", "datalake_inputs", "2021Q2")
    list_eikon_files <- list.files(path_db_eikon_data)
    # read eikon data from dropbox path as described in path_db_eikon_data
    list_eikon_data <- list_eikon_files %>%
      purrr::map(function(x) {
        message(paste0("Processing ", x))
        data <-
          readxl::read_xlsx(fs::path(path_db_eikon_data, x)) %>%
          janitor::clean_names(case = "snake")
      })
    list_eikon_data
  }
  # salvage archived data
  list_eikon_data <- get_old_eikon_data()
  eikon_data_old <-
    bind_rows(list_eikon_data) %>%
    mutate(
      net_profit_margin = dplyr::case_when(
        .data$credit_smart_ratios_net_profit_margin_percent_ltm_s_avg < 0 &
          dplyr::between(.data$net_profit_margin_percent_0d_ltm_1_s_avg, 0, 1) ~
          .data$net_profit_margin_percent_0d_ltm_1_s_avg,
        .data$credit_smart_ratios_net_profit_margin_percent_ltm_s_avg < 0 &
          .data$net_profit_margin_percent_0d_ltm_1_s_avg < 0 ~ 0,
        .data$credit_smart_ratios_net_profit_margin_percent_ltm_s_avg < 0 &
          .data$net_profit_margin_percent_0d_ltm_1_s_avg > 1 ~ 0,
        .data$credit_smart_ratios_net_profit_margin_percent_ltm_s_avg > 1 &
          dplyr::between(.data$net_profit_margin_percent_0d_ltm_1_s_avg, 0, 1) ~
          .data$net_profit_margin_percent_0d_ltm_1_s_avg,
        .data$credit_smart_ratios_net_profit_margin_percent_ltm_s_avg > 1 &
          .data$net_profit_margin_percent_0d_ltm_1_s_avg > 1 ~ 1,
        .data$credit_smart_ratios_net_profit_margin_percent_ltm_s_avg > 1 &
          .data$net_profit_margin_percent_0d_ltm_1_s_avg < 0 ~ 1,
        TRUE ~ .data$credit_smart_ratios_net_profit_margin_percent_ltm_s_avg
      ),
      pd = as.double(.data$x4),
      debt_equity_ratio =
        as.numeric(.data$leverage_s_avg),
      volatility = as.numeric(asset_volatility_s_avg),
      asset_drift = as.numeric(asset_drift_s_avg),
      ticker_symbol = identifier_ric,
      trbc_industry_name = gics_sub_industry_name
    ) %>%
    dplyr::select(
      .data$isin,
      .data$structural,
      ticker_symbol,
      pd,
      net_profit_margin,
      .data$debt_equity_ratio,
      .data$volatility,
      .data$asset_drift,
      trbc_industry_name
    ) %>%
    filter(!is.na(isin))

  # load recent data
  eikon_new <-
    readr::read_csv(file.path("data-raw", "datalake_inputs", "eikon_data.csv"), na = c("")) %>%
    dplyr::mutate(
      debt_equity_ratio = as.numeric(.data$credit_structural_leverage),
      corporate_bond_ticker = .data$ticker_symbol,
      pd = .data$`credit_structural_pd_%`,
      net_profit_margin = .data$`net_profit_margin_%`,
      volatility = .data$`credit_structural_asset_volatility_%`,
      asset_drift = `credit_structural_asset_drift_%`,
      bics_subgroup = .data$trbc_industry_name,
      bics_sector = .data$source_sheet,
    ) %>%
    dplyr::mutate(
      net_profit_margin = as.numeric(.data$net_profit_margin) / 100,
      volatility = as.numeric(.data$volatility) / 100,
      pd = as.numeric(.data$pd) / 100,
      asset_drift = as.numeric(asset_drift) / 100
    ) %>%
    dplyr::select(
      isin,
      ticker_symbol,
      pd,
      net_profit_margin,
      debt_equity_ratio,
      volatility,
      asset_drift,
      trbc_industry_name
    )

  eikon_new <- eikon_new %>%
    group_by(isin) %>%
    summarise(
      ticker_symbol = dplyr::first(ticker_symbol),
      pd = median(pd),
      net_profit_margin = median(net_profit_margin),
      debt_equity_ratio = median(debt_equity_ratio),
      volatility = median(volatility),
      asset_drift = median(asset_drift),
      trbc_industry_name = dplyr::first(trbc_industry_name)
    )

  DB_assets_eikon <-
    eikon_data_old %>%
    anti_join(eikon_new, by = "isin") %>%
    bind_rows(eikon_new) %>%
    mutate(ald_location = substr(isin, start = 1, stop = 2)) %>%
    select(-c(ticker_symbol, trbc_industry_name))
  DB_assets_eikon
}

make_asset_impact_db <- function() {
  read_asset_resolution <- function(path_ar_data_raw, sheet_name) {
    ar_data <- readxl::read_xlsx(path_ar_data_raw,
      sheet = sheet_name
    ) %>%
      dplyr::select(-dplyr::starts_with("Direct Ownership"), -.data$`Asset Region`) %>%
      dplyr::rename(
        id = .data$`Company ID`,
        company_name = .data$`Company Name`,
        ald_sector = .data$`Asset Sector`,
        technology = .data$`Asset Technology`,
        technology_type = .data$`Asset Technology Type`,
        ald_location = .data$`Asset Country`,
        activity_unit = .data$`Activity Unit`
      )
    return(ar_data)
  }

  # load asset_impact data -------
  # asset_impact_company_informations
  asset_impact_data <-
    readxl::read_excel(fs::path("data-raw", "datalake_inputs", "AR-Company-Indicators.xlsx"),
      sheet = "Company Information"
    ) %>%
    dplyr::select(c(-LEI)) %>%
    dplyr::rename(
      company_id = `Company ID`,
      company_name = `Company Name`,
      is_ultimate_parent = `Is Ultimate Parent`,
      country_of_domicile = `Country of Domicile`
    )

  company_activities <-
    read_asset_resolution(fs::path("data-raw", "datalake_inputs", "AR-Company-Indicators.xlsx"),
      sheet_name = "Company Activities"
    )

  DB_asset_impact <- asset_impact_data %>% full_join(
    company_activities %>%
      rename(company_id = id) %>%
      distinct(company_id, ald_sector, ald_location),
    by = c("company_id")
  )
  DB_asset_impact
}


make_ids_db <- function(DB_asset_impact, DB_assets_eikon) {
  # salvage archives
  # path_db_analysis_inputs <- fs::path(
  #   r2dii.utils::dbox_port_00(), "07_AnalysisInputs", "2020Q4_05172021_2020_MFM"
  # )
  path_db_analysis_inputs <- fs::path("data-raw", "datalake_inputs")
  # security financial data--------
  # read from the dropbox. produced by running the data_preparation repo
  security_financial_data <- readr::read_rds(fs::path(path_db_analysis_inputs, "security_financial_data", ext = "rda"))
  # consolidated financial data----
  # read from the dropbox. produced by running the data_preparation repo
  consolidated_financial_data <- readr::read_rds(fs::path(
    path_db_analysis_inputs,
    "consolidated_financial_data",
    ext = "rda"
  ))
  usable_old_data <-
    dplyr::inner_join(security_financial_data, consolidated_financial_data)

  # Load asset impact provided isins
  # bind rows with asset impact companies with no isin
  asset_impact_isins <-
    readxl::read_excel(fs::path("data-raw", "datalake_inputs", "AR-Company-Indicators.xlsx"),
      sheet = "Company ISINs"
    ) %>%
    dplyr::rename(
      company_id = `Company ID`,
      company_name = `Company Name`,
      isin = ISIN
    )
  asset_impact_isins <-
    bind_rows(
      asset_impact_isins,
      DB_asset_impact %>% distinct(company_id, company_name) %>% anti_join(asset_impact_isins)
    )


  # remove old data that exist in asset impact,
  # but only if an isin doesn't exist
  usable_old_data_no_new_asset_impact <-
    usable_old_data %>%
    distinct(
      isin,
      company_name,
      company_id,
      bloomberg_id,
      legal_entity_id,
      parent_company_id,
      obligor_company_id
    ) %>%
    anti_join(asset_impact_isins %>% distinct(company_id))


  DB_ids <- bind_rows(
    usable_old_data_no_new_asset_impact %>% anti_join(asset_impact_isins %>% distinct(isin)),
    asset_impact_isins %>% distinct(company_id, company_name, isin)
  )


  # Now add isins without a company_id
  # so add eikon isins only if they aren't in DB_ids
  DB_ids <- bind_rows(
    DB_ids,
    DB_assets_eikon %>% distinct(isin) %>% anti_join(DB_ids %>% distinct(isin))
  )

  DB_ids
}


make_ownership_tree_db <- function() {
  #' Prewrangle the ownership tree data from AR
  #'
  #' @param data A data frame holding the raw ownership tree data set
  #' @return NULL
  prewrangle_ownership_tree <- function(data) {
    # filter only one direction within ownership_tree
    ownership_tree <- data %>%
      dplyr::filter(.data$ownership_level >= 0)

    # only take the majority parent for each company at each ownership level
    # (otherwise it would get the profit margins from both parents, which one is the better one??)
    # Note: if linking_stake == NA in raw data, this means 100% is owned by one company
    ownership_tree <- ownership_tree %>%
      # need to change NAs to 100%, otherwise slice_max will kick them out
      dplyr::mutate(linking_stake = dplyr::if_else(is.na(.data$linking_stake), 100, .data$linking_stake)) %>%
      dplyr::group_by(.data$company_id, .data$ownership_level) %>%
      # slice the majority parent
      dplyr::slice_max(.data$linking_stake) %>%
      dplyr::ungroup() %>%
      # still have to take distinct in case the linking stake is equal (e.g. 50%/50%)
      dplyr::distinct(.data$company_id, .data$ownership_level, .keep_all = TRUE)
  }

  path_db_datastore <- fs::path(
    "data-raw",
    "datalake_inputs",
    "06_DataStore",
    "DataStore_export_05172021",
    "2020Q4"
  )
  # ownership_tree-----------------
  # read from the dropbox. file provided by AR. clarify interval of releases.
  ownership_tree <- readr::read_csv(
    file.path(
      "data-raw",
      "datalake_inputs",
      "company_ownership_bidirectional.csv"
    ),
    col_types = readr::cols_only(
      target_company_id = "d",
      company_id = "d",
      linking_stake = "d",
      ownership_level = "d"
    )
  )
  # make sure ownership structures are unique
  ownership_tree <- ownership_tree %>%
    dplyr::distinct_all()

  prewrangled_ownership_tree <-
    prewrangle_ownership_tree(ownership_tree)

  prewrangled_ownership_tree <- prewrangled_ownership_tree %>%
    rename(
      parent_company_id = target_company_id,
      subsidiary_company_id = company_id
    )

  prewrangled_ownership_tree
}

get_additional_isins <- function(DB_ids) {
  new_ar_id_and_isins <-
    readxl::read_excel(file.path("data-raw", "datalake_inputs", "CDI ISINs.xlsx")) %>%
    rename(company_id = ar_company_id) %>%
    distinct(company_id, isin)

  unknown_isins <- new_ar_id_and_isins %>%
    anti_join(DB_ids %>%
      distinct(isin))

  DB_ids_no_isins <- DB_ids %>%
    distinct_at(vars(-isin))

  unknown_isins_enhanced <- unknown_isins %>%
    left_join(DB_ids_no_isins,
      by = join_by(company_id),
      relationship = "many-to-many"
    ) %>%
    mutate(trustworthy = T)

  DB_ids_enhanced <- bind_rows(
    DB_ids,
    unknown_isins_enhanced
  )

  DB_ids_enhanced
}


# ================ CHECK CONSISTENCY & SAVE

# primary_key: isin
# company_id company_name is_ultimate_parent
# country_of_domicile isin ald_location ald_sector
DB_asset_impact <- make_asset_impact_db() %>%
  dplyr::filter(company_name != "Unknown")
# primary key: isin
# columns : isin structural ticker_symbol pd net_profit_margin
# debt_equity_ratio volatility asset_drift
# trbc_industry_name data_version ald_location
DB_assets_eikon <- make_eikon_db() %>%
  select(-c(structural))
# primary key:  isin
# columns: company_id bloomberg_id legal_entity_id
# parent_company_id obligor_company_id isin, trustworthy
# trustworthy refers to the fact if the data source can be trusted or not
DB_ids <- make_ids_db(DB_asset_impact, DB_assets_eikon)
DB_ids <- get_additional_isins(DB_ids)


# this block makes sure that for a given company, NA isins
# exist only if no other isin is present.
# i.e. a company can have either 1 row with NA isin, or multiple rows with multiple isins
DB_ids_no_nan_isin <- DB_ids %>%
  filter(!is.na(isin))
DB_ids_nan_isin <- DB_ids %>%
  filter(is.na(isin)) %>%
  anti_join(DB_ids_no_nan_isin, by = "company_id")
DB_ids <- bind_rows(
  DB_ids_no_nan_isin,
  DB_ids_nan_isin
) %>%
  assertr::verify(length(unique(.$company_id))
  == length(unique(DB_ids$company_id)))

# Check that an isin is associated to only 1 company_id
DB_ids %>%
  assertr::verify(max(DB_ids %>% distinct(isin, company_id) %>%
    group_by(isin, company_id) %>%
    summarise(nrow = n()) %>%
    pull(nrow)) == 1)

# check DB_ids contains ids from all other tables
DB_asset_impact %>%
  distinct(company_id) %>%
  inner_join(DB_ids %>% distinct(company_id)) %>%
  assertr::verify(nrow(.) == nrow(DB_asset_impact %>%
    distinct(company_id)))

DB_assets_eikon %>%
  distinct(isin) %>%
  inner_join(DB_ids %>% distinct(isin)) %>%
  assertr::verify(nrow(.) == nrow(DB_assets_eikon %>%
    distinct(isin)))

# check volumes
DB_assets_eikon %>%
  inner_join(DB_ids %>% distinct(isin, company_id)) %>%
  inner_join(DB_asset_impact,
    by = join_by(company_id)
  )


# create last database, probably will be dropped in the future because not provided anymore
# columns: target_company_id company_id linking_stake ownership_level
DB_ownership_tree <- make_ownership_tree_db()


path_ar_data_raw <-
  r2dii.utils::path_dropbox_2dii(
    "ST_INPUTS",
    "ST_INPUTS_PRODUCTION",
    "AR-Company-Indicators_2022Q4.xlsx"
  )

outputs_list <- prepare_asset_impact_data(ar_data_path=path_ar_data_raw)
DB_company_activities <- outputs_list[["company_activities"]]
DB_company_emissions <- outputs_list[["company_emissions"]]

output_dir <- fs::path("data-raw", "DBs")
dir.create(output_dir, showWarnings = F, recursive = T)


DB_company_activities %>% arrow::write_parquet(fs::path(output_dir,"DB_company_activities", ext="parquet"))
DB_company_emissions %>% arrow::write_parquet(fs::path(output_dir,"DB_company_emissions", ext="parquet"))
DB_assets_eikon %>% arrow::write_parquet(fs::path(output_dir, "DB_assets_eikon.parquet"))
DB_asset_impact %>% arrow::write_parquet(fs::path(output_dir, "DB_asset_impact.parquet"))
DB_ids %>% arrow::write_parquet(fs::path(output_dir, "DB_ids.parquet"))
DB_ownership_tree %>% arrow::write_parquet(fs::path(output_dir, "DB_ownership_tree.parquet"))

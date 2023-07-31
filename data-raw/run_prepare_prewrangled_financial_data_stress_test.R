devtools::load_all()

eikon_data <-
  readr::read_csv(
    r2dii.utils::path_dropbox_2dii("ST_INPUTS", "ST_PRODUCTION", "eikon_data_enhanced_ids.csv")
  )
abcd_data <-
  readr::read_csv(
    r2dii.utils::path_dropbox_2dii("ST_INPUTS", "ST_PRODUCTION", "abcd_stress_test_input.csv")
  )

eikon_data <- eikon_data %>%
  dplyr::rename(
    debt_equity_ratio = credit_structural_leverage,
    # TODO OK ?
    corporate_bond_ticker = ticker_symbol,
    pd = `credit_structural_pd_%`,
    net_profit_margin = `net_profit_margin_%`,
    volatility = `credit_structural_asset_volatility_%`
  ) %>%
  dplyr::mutate(
    net_profit_margin = net_profit_margin / 100,
    volatility = volatility / 100,
    pd = pd / 100
  ) %>%
  dplyr::select(
    company_id,
    trbc_industry_name,
    corporate_bond_ticker,
    company_name,
    debt_equity_ratio,
    pd,
    net_profit_margin,
    volatility
  )


# parameters for minimum requirements to reference subgroups in creating averages
# determine size of subgroup below which we do not use the average because the
# minimum required sample size of reference subgroup
n_min_sample <- 50
# minimum required ratio of reference subgroup to sample
min_ratio_sample_subgroup <- 1 / 3
# cut off values for profit margins
range_profit_margin <- c(-Inf, Inf)
eikon_trbc_averages <- create_averages_eikon_sector(
  eikon_data,
  minimum_sample_size = n_min_sample,
  minimum_ratio_sample = min_ratio_sample_subgroup,
  allowed_range_npm = range_profit_margin
)

abcd_sectors_averages <-
  create_averages_abcd_sectors(eikon_data, abcd_data, eikon_trbc_averages)

eikon_full <-
  expand_eikon_on_abcd(eikon_data, abcd_data, abcd_sectors_averages)

eikon_full <- eikon_full %>%
  dplyr::select(c(-.data$trbc_industry_name)) %>%
  dplyr::left_join(abcd_data %>% distinct(.data$company_id, .data$company_name))

eikon_full %>% readr::write_csv(file.path("data-raw", "prewrangled_financial_data_stress_test.csv"))

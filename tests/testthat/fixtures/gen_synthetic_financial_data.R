library(dplyr)
library(tibble)

# TODO with those columns
# company_id, company_name, corporate_bond_ticker, pd, net_profit_margin, debt_equity_ratio, volatility

prewrangled_financial_data_stress_test <- abcd_stress_test_input %>%
  dplyr::distinct(company_id, company_name) %>%
  tibble::add_column(
    corporate_bond_ticker = NA,
    pd = runif(nrow(.), min = 0, max = 1),
    net_profit_margin = runif(nrow(.), min = 0, max = 1),
    debt_equity_ratio = runif(nrow(.), min = 0, max = 1),
    volatility = runif(nrow(.), min = 0, max = 1)
  )

usethis::use_data(prewrangled_financial_data_stress_test, overwrite = TRUE)

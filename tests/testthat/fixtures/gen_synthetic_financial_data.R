
library(dplyr)
library(tibble)

# TODO with those columns
# company_id, company_name, corporate_bond_ticker, pd, net_profit_margin, debt_equity_ratio, volatility
load("data/company_activities.rda")
available_companies <- company_activities  %>% dplyr::distinct(company_id) %>% dplyr::pull()

# random sample of companies  
sample_companies <- sample(available_companies, size=length(available_companies)*10, replace = TRUE)  
#remove 50% companies at random
sample_companies <- sample_companies[!(sample_companies %in% 
  sample(available_companies, size=as.integer(length(available_companies)/2), replace=FALSE)
  )]

  

random_isin <- function(...) {
  countries <- countrycode::codelist %>%
    filter(!is.na(iso2c)) %>%
    distinct(iso2c) %>%
    pull(iso2c)

  sprintf(
    "%s%s",
    paste0(sample(countries, 1, TRUE), collapse = ""),
    paste0(sample(9, 10, TRUE), collapse = "")
  )
}

random_isins_with_companies <- purrr::map_vec(1:length(sample_companies), random_isin)
random_isins_no_companies <- purrr::map_vec(1:100, random_isin)
random_isins <- tibble(isin=c(random_isins_with_companies, random_isins_no_companies))
stopifnot(nrow(random_isins) == length(unique(random_isins$isin))) # check all isin unique



  tibble::add_column(
    # corporate_bond_ticker = NA,
    pd = runif(nrow(.), min = 0, max = 1),
    net_profit_margin = runif(nrow(.), min = 0, max = 1),
    debt_equity_ratio = runif(nrow(.), min = 0, max = 1),
    volatility = runif(nrow(.), min = 0, max = 1)
  )

usethis::use_data(prewrangled_financial_data_stress_test, overwrite=TRUE)

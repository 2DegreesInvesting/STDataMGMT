
devtools::load_all()
library(dplyr)
library(readr)

portfolio_values <- company_activities %>%
  select(company_id, company_name, ald_sector, ald_business_unit, ald_location) %>%
  mutate(value_usd=sample(1e4:1e9, n(), replace = TRUE),
         term=sample(1:5, n(), replace = TRUE))


companies_bonds <-
  portfolio_values %>%
  sample_n(200) %>%
  mutate(asset_type = "Bonds")

companies_equities <-
  portfolio_values %>%
  sample_n(200) %>%
  mutate(
    asset_type = "Equity"
  )

# companies_id <-
#   all_crispy %>%
#   distinct(company_id, sector, business_unit) %>%
#   sample_n(200) %>%
#   rename(
#     ald_sector = sector,
#     ald_business_unit = business_unit
#   )

# companies_bonds <-
#   bind_cols(companies[sample(1:nrow(companies)), ], bonds)
# companies_equities <-
#   bind_cols(companies[sample(1:nrow(companies)), ], equities)

portfolio_data <- bind_rows(companies_bonds, companies_equities)
# portfolio_data <- portfolio_data %>%
#   mutate(
#     first_maturity = as.Date(portfolio_data$first_maturity, "%d.%m.%y")
#   )
portfolio_data[, "loss_given_default"] <- runif(n = nrow(portfolio_data), min = 1e-12, max = .9999999999)


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

portfolio_data[, "isin"] <- purrr::map_vec(1:nrow(portfolio_data), random_isin)

portfolio_data <- portfolio_data %>%
  mutate(ald_location = substring(isin, 1, 2))

portfolio_data[rbinom(nrow(portfolio_data), 1, 0.3) == 1, "isin"] <- NA

portfolio_data <- portfolio_data %>% select(
  company_id,
  isin,
  ald_sector,
  ald_business_unit,
  ald_location,
  asset_type,
  value_usd,
  term,
  loss_given_default,
)

usethis::use_data(portfolio_data)

devtools::load_all()
library(dplyr)

company_activities <- load("data/company_activities.rda")

portfolio_values <-
 company_activities %>%
  select(company_id, company_name, ald_sector, ald_business_unit, ald_location) %>%
  mutate(
    value_usd = sample(1e4:1e9, n(), replace = TRUE),
    term = sample(1:5, n(), replace = TRUE)
  )


companies_fixed_income <-
  portfolio_values %>%
  sample_n(200) %>%
  mutate(asset_type = "fixed_income") %>%

companies_equities <-
  portfolio_values %>%
  sample_n(200) %>%
  mutate(
    asset_type = "equity"
  )

# companies_id <-
#   all_crispy %>%
#   distinct(company_id, sector, business_unit) %>%
#   sample_n(200) %>%
#   rename(
#     ald_sector = sector,
#     ald_business_unit = business_unit
#   )

# companies_fixed_income <-
#   bind_cols(companies[sample(1:nrow(companies)), ], bonds)
# companies_equities <-
#   bind_cols(companies[sample(1:nrow(companies)), ], equities)

portfolio_data <- bind_rows(companies_fixed_income, companies_equities)
# portfolio_data <- portfolio_data %>%
#   mutate(
#     first_maturity = as.Date(portfolio_data$first_maturity, "%d.%m.%y")
#   )
portfolio_data[, "loss_given_default"] <- runif(n = nrow(portfolio_data), min = 1e-12, max = .9999999999)


countries <- countrycode::codelist %>%
  filter(!is.na(iso2c)) %>%
  distinct(iso2c) %>%
  pull(iso2c)
portfolio_data <- portfolio_data %>%
  mutate(ald_location = if_else(!is.na(isin), substring(isin, 1, 2), sample(countries, 1, TRUE)))

# portfolio_data[rbinom(nrow(portfolio_data), 1, 0.3) == 1, "isin"] <- NA

portfolio_data <- portfolio_data %>% select(
  asset_id, 
  company_id,
  asset_type,
  ald_sector,
  ald_business_unit,
  ald_location,
  exposure_value_usd,
  term_date,
  loss_given_default,
)
(~asset_id, ~asset_type, ~ald_sector, ~ald_business_unit, ~ald_location, ~exposure_value_usd, ~expiration_date)

usethis::use_data(portfolio_data, overwrite = TRUE)

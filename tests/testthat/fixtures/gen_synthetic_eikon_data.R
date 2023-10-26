devtools::load_all()

ISIN_COUNTRYCODES <- countrycode::codelist %>%
  dplyr::filter(!is.na(.data$ecb)) %>%
  dplyr::distinct(ecb) %>%
  dplyr::pull()

create_random_isins <- function(n_assets) {
  random_isin <- function() {
    sprintf(
      "%s%s",
      paste0(sample(ISIN_COUNTRYCODES, 1, TRUE), collapse = ""),
      paste0(sample(9, 10, TRUE), collapse = "")
    )
  }
  return(replicate(n_assets, random_isin()))
}


company_ids <- company_activities %>%
  select(company_id, ald_sector, ald_business_unit, ald_location)



# companies_with_isins <- sample(unique(company_ids %>% pull(company_id)), 10, replace=FALSE)
# companies_with_isins <- tibble::tribble(~isin, )
# companies_with_isins_at_locations <-

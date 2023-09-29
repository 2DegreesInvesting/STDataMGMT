load_portfolio_data <- function(portfolio_input_path) {
  portfolio_data <- readr::read_csv(
    portfolio_input_path,
    col_types = readr::cols_only(
      company_id = "i",
      isin = "c",
      ald_sector = "c",
      ald_business_unit = "c",
      ald_location = "c",
      asset_type = "c",
      value_usd = "d",
      first_maturity = col_date("%Y-%m-%d"),
      loss_given_default = "d"
    )
  )
  return(portfolio_data)
}


#' Find the index of the largest year in a vector that is less than or equal to a given year.
#'
#' This function compares a given year to a vector of years and returns the index
#' of the largest year in the vector that is less than or equal to the given year.
#'
#' @param year The year to compare.
#' @param vec_years A vector of years to compare against.
#'
#' @return The index of the largest year in vec_years that is less than or equal to the given year.
#' If no such year is found, returns NA.
#'
#' @author chatgpt
#'
#' @examples
#' year <- 2005
#' vec_years <- c(1999, 2003, 2006, 2010, 2001)
#' find_largest_year_index(year, vec_years)
#'
#' @export
find_largest_year_index <- function(year, vec_years) {
  # Filter the vector to include only years smaller than or equal to the given year
  filtered_years <- vec_years[vec_years <= year]

  # If no years are smaller than or equal to the given year, return NA
  if (length(filtered_years) == 0) {
    return(NA)
  }

  # Find the index of the largest year in the filtered vector
  largest_index <- which.max(filtered_years)

  # Return the index of the largest year in the original vector
  return(largest_index)
}


convert_maturity_to_term <- function(date_vec, maturity_year_term) {
  maturities_year <- as.integer(format(date_vec, format = "%Y"))
  term_vector <- purrr::map_vec(
    maturities_year,
    ~ ifelse(!is.na(.),
      find_largest_year_index(., maturity_year_term),
      5
    )
  )
  return(term_vector)
}


preprocess_portfolio_data <-
  function(portfolio_data,
           maturity_year_term,
           bench_regions) {
    portfolio_data[, "term"] <-
      convert_maturity_to_term(
        portfolio_data$first_maturity,
        maturity_year_term
      )
    portfolio_data <- expand_by_scenario_geography(portfolio_data, bench_regions)

    return(portfolio_data)
  }

#' Prepare example data
#'
#' @param data A Tibble holding example data.
#'
#' @return NULL
prepare_example_data <- function(data) {
  # example custom rule that we need to that we need to to because xyz
  data <- data %>%
    dplyr::filter(.data$some_col < 5)

  return(data)
}

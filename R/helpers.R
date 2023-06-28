#' Report missing
#'
#' Function reports number of missing values per variable.
#'
#' @param data Tibble holding a data set.
#' @param name_data Name of the data file.
#' @param throw_error Boolean, indicating if error shall be thrown if missings
#'   are detected.
#'
#' @return input `data`.
report_missings <- function(data, name_data, throw_error = TRUE) {
  missings <- purrr::map_df(data, function(x) sum(is.na(x)))

  cat("Reporting missings on dataset:", name_data, "\n")
  purrr::iwalk(missings, function(n_na, name) {
    cat("Counted", n_na, "missings on column", name, "\n")
  })
  cat("\n\n")

  if (throw_error && rowSums(missings) > 0) {
    stop(paste0("Missings detected on ", name_data, ", please check dataset."), call. = FALSE)
  }

  invisible(data)
}

#' Report duplicate rows
#'
#' Reports duplicates in `data` on columns `cols`. Duplicates are reported via a
#' warning.
#'
#' @inheritParams report_missings
#' @param cols Cols to check for duplicate combinations on.
#'
#' @return NULL
report_duplicates <- function(data, cols, throw_error = TRUE) {
  duplicates <- data %>%
    dplyr::group_by(!!!rlang::syms(cols)) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::select(!!!rlang::syms(cols)) %>%
    dplyr::distinct_all()

  if (nrow(duplicates) > 0) {
    if (throw_error) {
      stop(paste0("Identified ", nrow(duplicates), " duplicates on columns ", paste(cols, collapse = ", "), "."))
    } else {
      warning(paste0("Identified ", nrow(duplicates), " duplicates on columns ", paste(cols, collapse = ", "), "."), call. = FALSE)
    }
  }

  return(invisible())
}

#' Identify and report missing value combinations
#'
#' Checks if all level combinations of `cols` are in`data` and
#' throws a warning on missing combinations.
#' NOTE:
#' 1. a combination of all levels is not necessarily required/useful, make sure
#' to use function only in adequate context.
#' 1. combinations of too many columns/values may exceed memory size.
#' .
#'
#' @inheritParams report_duplicates
#' @param cols Vector holding name of columns
#'
#' @return NULL
report_missing_col_combinations <- function(data, cols, throw_error = FALSE) {
  all_combinations <- data %>%
    tidyr::expand(!!!rlang::syms(cols))

  missing_rows <- all_combinations %>%
    dplyr::anti_join(data, by = cols)

  if (nrow(missing_rows) > 0) {
    if (throw_error) {
      stop(paste0("Identified ", nrow(missing_rows), " missing combinations on columns ", paste(cols, collapse = ", "), "."))
    } else {
      warning(paste0("Identified ", nrow(missing_rows), " missing combinations on columns ", paste(cols, collapse = ", "), "."), call. = FALSE)
    }
  }

  return(invisible())
}

#' Identify how many rows are added or removed due to a specific operation
#'
#' @param data A data frame for which to indicate differences in the number of
#'   rows pre and post an operation
#' @param initial_n_rows A numeric vector of length one that indicates the
#'   number of rows before the application of the operation at hand. Usually,
#'   this can get nrow(data) as an input.
#' @param cause A string that clarifies the reason for a difference in rows for
#'   the application at hand.
#'
#' @return NULL
report_diff_rows <- function(data, initial_n_rows, cause = "") {
  diff <- initial_n_rows - nrow(data)
  if (diff > 0) cat(crayon::red("\n", diff, "rows have been removed", cause, "\n", "\n"))
  if (diff == 0) cat(crayon::green("\n", "Number of rows has not changed", cause, "\n", "\n"))
  if (diff < 0) cat(crayon::blue("\n", -diff, "rows have been added", cause, "\n", "\n"))

  return(data)
}

# Remove all scenario x scenario_geography x sectors combinations from data for
# which not all expected technologies per sector (as defined by mapper
# p4i_p4b_sector_technology_lookup) are available.
# Mapper with expected levels can handle p4_type P4I and P4B
remove_incomplete_sectors <- function(data, p4_type = "P4I") {
  if (!p4_type %in% c("P4I", "P4B")) {
    stop("Only supporting p4_types P4I and P4B")
  }

  complete_combinations <- data %>%
    split(list(.$scenario, .$scenario_geography)) %>%
    purrr::map_dfr(function(x) {
      # right joining on lookup so that all missing sectors/sector x technology
      # combinations within a scenario geography are identifiable via NAs
      if (p4_type == "P4I") {
        p4i_p4b_sector_technology_lookup_df <- p4i_p4b_sector_technology_lookup()

        joined <- x %>%
          dplyr::right_join(
            p4i_p4b_sector_technology_lookup_df %>%
              dplyr::select(.data$sector_p4i, .data$technology_p4i),
            by = c(
              "ald_sector" = "sector_p4i",
              "technology" = "technology_p4i"
            )
          )
      } else {
        joined <-
          x %>% dplyr::right_join(
            p4i_p4b_sector_technology_lookup_df %>%
              dplyr::select(.data$sector_p4b, .data$technology_p4b),
            by = c(
              "ald_sector" = "sector_p4b",
              "technology" = "technology_p4b"
            )
          )
      }

      sectors_with_nas <- joined %>%
        dplyr::filter(is.na(.data$scenario_geography)) %>%
        dplyr::pull(.data$ald_sector) %>%
        unique()

      complete_sector <- x %>%
        dplyr::filter(!.data$ald_sector %in% sectors_with_nas)
    })

  return(complete_combinations)
}

#' Apply a rename mapping on a tibble column
#' @param data A tibble
#' @param colname name of the column with values to be renamed
#' @param key_value_mapping A named vector describing the mapping, keys as old name, values as new name
#'
#' @return input dataframe `data` with `colname` column values renamed according to `key_value_mapping`
rename_column_values <- function(data, colname, key_value_mapping) {
  data[[colname]] <- purrr::map_vec(
    data[[colname]],
    function(x) ifelse(x %in% names(key_value_mapping), key_value_mapping[[x]], x)
  )
  return(data)
}

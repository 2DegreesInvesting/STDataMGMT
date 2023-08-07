#' Prewrangle the prepared eikon data into the format used by the stress test
#'
#' @param eikon_data A data frame which contains the prepared eikon data as
#'   provided after running prepare_eikon_data()
#'
#' @return A list of three data.frames
prepare_prewrangled_financial_data_stress_test <- function(eikon_data) {
  # ... ADO 2563 - temporarily remove all production related data from master data
  # in a next version, we will simply split financial data and production data in
  # two separate, but closely related files.
  # for now, we use distinct_all after removing the production data and check there
  # are no duplicates
  # ... aggregate to the ticker/company level
  financial_data_stress_test <- eikon_data %>%
    dplyr::select(
      .data$company_name, .data$company_id, .data$corporate_bond_ticker,
      .data$final_pd,
      .data$final_net_profit_margin, .data$final_debt_equity_ratio,
      .data$final_volatility
    )

  names(financial_data_stress_test) <- names(financial_data_stress_test) %>%
    stringr::str_remove_all("final_")

  # set financial data net_profit_margin values between 0 and 1
  financial_data_stress_test <- financial_data_stress_test %>%
    dplyr::mutate(
      net_profit_margin = dplyr::case_when(
        .data$net_profit_margin > 1 ~ 1,
        .data$net_profit_margin < 0 ~ 0,
        TRUE ~ net_profit_margin
      )
    )

  # remove rows where net_profit_margin <= 0
  financial_data_stress_test <- financial_data_stress_test %>%
    dplyr::filter(.data$net_profit_margin > 0) %>%
    dplyr::select(
      .data$company_name, .data$company_id, .data$corporate_bond_ticker,
      .data$pd, .data$net_profit_margin, .data$debt_equity_ratio,
      .data$volatility
    )

  # TODO: any logic/bounds needed for debt/equity ratio and volatility?

  return(financial_data_stress_test)
}

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
      .data$final_pd, .data$final_profit_margin_preferred,
      .data$final_profit_margin_unpreferred, .data$final_leverage_s_avg,
      .data$final_asset_volatility_s_avg
    )

  names(financial_data_stress_test) <- names(financial_data_stress_test) %>%
    stringr::str_remove_all("final_")

  financial_data_stress_test <- financial_data_stress_test %>%
    dplyr::distinct_all() %>%
    dplyr::mutate(net_profit_margin = .data$profit_margin_preferred) %>%
    # TODO: logic unclear thus far
    dplyr::mutate(
      net_profit_margin = dplyr::case_when(
        .data$net_profit_margin < 0 &
          dplyr::between(.data$profit_margin_unpreferred, 0, 1) ~
          .data$profit_margin_unpreferred,
        .data$net_profit_margin < 0 & .data$profit_margin_unpreferred < 0 ~ 0,
        .data$net_profit_margin < 0 & .data$profit_margin_unpreferred > 1 ~ 0,
        .data$net_profit_margin > 1 &
          dplyr::between(.data$profit_margin_unpreferred, 0, 1) ~
          .data$profit_margin_unpreferred,
        .data$net_profit_margin > 1 & .data$profit_margin_unpreferred > 1 ~ 1,
        .data$net_profit_margin > 1 & .data$profit_margin_unpreferred < 0 ~ 1,
        TRUE ~ .data$net_profit_margin
      )
    )

  financial_data_stress_test_rm_profit_margin <- financial_data_stress_test %>%
    dplyr::filter(.data$net_profit_margin <= 0) %>%
    dplyr::select(
      .data$company_name, .data$company_id, .data$corporate_bond_ticker,
      .data$net_profit_margin, .data$profit_margin_unpreferred
    )

  financial_data_stress_test <- financial_data_stress_test %>%
    dplyr::filter(.data$net_profit_margin > 0) %>%
    dplyr::rename(
      debt_equity_ratio = .data$leverage_s_avg,
      volatility = .data$asset_volatility_s_avg
    ) %>%
    dplyr::select(
      .data$company_name, .data$company_id, .data$corporate_bond_ticker,
      .data$pd, .data$net_profit_margin, .data$debt_equity_ratio,
      .data$volatility
    )

  financial_data_stress_test_rm_profit_margin <- financial_data_stress_test_rm_profit_margin %>%
    dplyr::filter(.data$net_profit_margin <= 0) %>%
    dplyr::select(
      .data$company_name, .data$company_id, .data$corporate_bond_ticker,
      .data$net_profit_margin, .data$profit_margin_unpreferred
    )

  financial_data_stress_test_rm_non_ascii <- financial_data_stress_test %>%
    dplyr::mutate(enc = stringi::stri_enc_mark(.data$company_name)) %>%
    dplyr::filter(.data$enc != "ASCII") %>%
    dplyr::select(-.data$enc)

  # financial_data_stress_test <- financial_data_stress_test %>%
  #   dplyr::mutate(enc = stringi::stri_enc_mark(.data$company_name)) %>%
  #   dplyr::filter(.data$enc == "ASCII") %>%
  #   dplyr::select(-.data$enc)

  # TODO: any logic/bounds needed for debt/equity ratio and volatility?

  list_prewrangled_financial_data <- list(
    financial_data_stress_test,
    financial_data_stress_test_rm_profit_margin,
    financial_data_stress_test_rm_non_ascii
  )
}

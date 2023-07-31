#' Create averages of eikon variables for companies with missing values
#'
#' @description This function uses the median for averaging numeric variables
#' because the results may otherwise be very sensitive to outliers. Averages are
#' created only based on values we previously obtained directly from eikon.
#' @param data A data frame holding processed and possibly filtered Eikon data
#' @param minimum_sample_size A numeric vector of length one that determines the
#'   minimum required absolute sample size of the given subgroup. Below this
#'   value we do not calculate an average for this sub group.
#' @param minimum_ratio_sample A numeric vector of length one that determines
#'   the minimum required ratio of sub group to overall sample. Below this
#'   value we do not calculate an average for this sub group.
#' @param allowed_range_npm A numeric vector indicating the allowed minimum and
#'   maximum values for the net profit margin
#'
#' @return A data frame
#' @export
create_averages_eikon_sector <- function(data,
                                         minimum_sample_size,
                                         minimum_ratio_sample,
                                         allowed_range_npm) {
  eikon_trbc_averages <- data %>%
    dplyr::group_by(.data$trbc_industry_name) %>%
    dplyr::mutate(size_subgroup = dplyr::n()) %>%
    dplyr::summarise(
      size_subgroup = mean(.data$size_subgroup, na.rm = TRUE),
      size_sample = dplyr::n(),
      ratio_sample_subgroup = .data$size_sample / .data$size_subgroup,
      sample_sufficient = dplyr::if_else(.data$size_sample > .env$minimum_sample_size, TRUE, FALSE),
      ratio_sufficient = dplyr::if_else(
        .data$ratio_sample_subgroup > .env$minimum_ratio_sample,
        TRUE,
        FALSE
      ),
      avg_pd = stats::median(.data$pd, na.rm = TRUE),
      avg_volatility = stats::median(.data$volatility, na.rm = T),
      avg_net_profit_margin = stats::median(.data$net_profit_margin, na.rm = T),
      avg_debt_equity_ratio = stats::median(.data$debt_equity_ratio, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(dplyr::across(dplyr::contains("avg_"), ~ (!is.na(.))),
                  dplyr::across(dplyr::contains("avg_"), ~ (!is.infinite(.)))) %>%
    dplyr::filter(.data$sample_sufficient == TRUE |
                    .data$ratio_sufficient == TRUE) %>%
    dplyr::filter(dplyr::between(
      .data$avg_net_profit_margin,
      min(.env$allowed_range_npm, na.rm = TRUE),
      max(.env$allowed_range_npm, na.rm = TRUE)
    ))
  return(eikon_trbc_averages)
}

#' Creates the averages to fill missing values where no match is found between abcd and eikon
#' 1. join eikon and abcd to extract companies matched
#' 2. count the number of ald_sector/technology pair for each eikon trbc industry
#' 3. join the `eikon_trbc_averages` to get the financial replacement values for
#'    ald_sector/technology pairs depending with which eikon trbc they match
#' 4. compute the median of financial variables over each ald_sector/technology pair.
#'    This median is ponderated by the number of companies in abcd that have been matched
#'    with the eikon trbc, since the `eikon_trbc_averages` are computed for each trbc.
#'
#' @export
create_averages_abcd_sectors <-
  function(eikon_data,
           abcd_data,
           eikon_trbc_averages) {
    matched_companies <-
      eikon_data %>% dplyr::inner_join(abcd_data %>% dplyr::distinct(company_id, ald_sector, technology),
                                       by = "company_id")

    # # subgroups volume check debug
    # matched_companies %>%
    #   group_by(trbc_industry_name, ald_sector, technology) %>%
    #   summarise(nrow = n()) %>%
    #   arrange(desc(nrow)) %>%
    #   print(n = 1000)

    abcd_sectors_averages <- matched_companies %>%
      dplyr::group_by(.data$trbc_industry_name,
                      .data$ald_sector,
                      .data$technology) %>%
      dplyr::summarise(ngroup_match = dplyr::n()) %>%
      dplyr::left_join(
        eikon_trbc_averages %>% dplyr::select(
          .data$trbc_industry_name,
          .data$avg_pd,
          .data$avg_volatility,
          .data$avg_net_profit_margin,
          .data$avg_debt_equity_ratio
        ),
        by = "trbc_industry_name"
      ) %>%
      dplyr::group_by(ald_sector, technology) %>%
      dplyr::summarise(
        avg_pd = spatstat.geom::weighted.median(.data$avg_pd, w = .data$ngroup_match, na.rm = T),
        avg_volatility = spatstat.geom::weighted.median(
          .data$avg_volatility,
          w = .data$ngroup_match,
          na.rm = T
        ),
        avg_net_profit_margin = spatstat.geom::weighted.median(
          .data$avg_net_profit_margin,
          w = .data$ngroup_match,
          na.rm = T
        ),
        avg_debt_equity_ratio = spatstat.geom::weighted.median(
          .data$avg_debt_equity_ratio,
          w = .data$ngroup_match,
          na.rm = T
        )
      )

    # check values coherence:
    # subgroup_averages %>% filter(grepl("Electric", trbc_industry_name)) %>% print(width=Inf)
    # subgroup_averages %>% filter(grepl("Oil", trbc_industry_name)) %>% print(width=Inf)
    # ==> Oil PD < Electric PD like in eikon_trbc_averages

    return(abcd_sectors_averages)
  }

#' infer values on abcd where eikon does not match
#' companies are matched to the `abcd_sectors_averages` on their sector/tech pairs
#' financial variables are average for company with multiple sector/tech
#'
#' @export
expand_eikon_on_abcd <-
  function(eikon_data,
           abcd_data,
           abcd_sectors_averages) {
    matched_companies <-
      eikon_data %>% dplyr::inner_join(abcd_data %>% dplyr::distinct(.data$company_id),
                                       by = "company_id")
    not_matched_companies <- abcd_data %>%
      dplyr::filter(!.data$company_id %in% matched_companies[["company_id"]])

    inferred_companies <- not_matched_companies %>%
      dplyr::distinct(.data$company_id, .data$ald_sector, .data$technology) %>%
      dplyr::left_join(abcd_sectors_averages, by = c("ald_sector", "technology")) %>%
      dplyr::group_by(.data$company_id) %>%
      dplyr::summarise(
        debt_equity_ratio = mean(.data$avg_debt_equity_ratio, na.rm = T),
        pd = mean(.data$avg_pd, na.rm = T),
        net_profit_margin = mean(.data$avg_net_profit_margin, na.rm =
                                   T),
        volatility = mean(.data$avg_volatility, na.rm = T)
      )

    eikon_full <-
      dplyr::bind_rows(matched_companies, inferred_companies)

    return(eikon_full)
  }

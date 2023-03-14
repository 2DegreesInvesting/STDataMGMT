ticket_5\_match abcd_financial_dara exploration
================
2023-03-02

\#1.Count uniqe company_name in abcd_stress_test (42 500)

    ## [1] 42500     1

\#2.Count uniqe company_name in financial_data (50 492)

    ## [1] 50492     1

\#3.Match on company names (12859)

    ## [1] 12859     1

\#4. matching over id (and keep company name) (27715)

    ## [1] 27715     1

\#5. full trajectory is the df that is genereated in the stress test
when production data and financial data are matched

\#6. count full trajectory company names (11 500)

    ## [1] 11500     1

\#7. full trajectory after is the df that is genereated in the stress
test when production data and financial data are matched only on id

\#8. count full trajectory after company names (25055)

    ## [1] 25055     1

\##RESULTS \* matching over ids instead of company_name and id increases
coverage to 27715 \* lets see now what the result would be using the
tilt matcher

``` r
crucial_names <- c("id", "company_name")

financial_data_reduced <- financial_data_reduced %>%
  dplyr::rename(
    "id" = "company_id"
  )

check_crucial_names(financial_data_reduced, crucial_names)
report_duplicates(financial_data_reduced, crucial_names)
```

``` r
financial_data_reduced <- financial_data_reduced %>%
  dplyr::mutate(company_alias = to_alias(company_name))

knitr::kable(head(financial_data_reduced))
```

| company_name                           |     id | company_alias               |
|:---------------------------------------|-------:|:----------------------------|
| Jamuna Oil Co Ltd                      |  13238 | jamunaoilco ltd             |
| Bangladesh Petroleum Corp              | 218329 | bangladeshpetroleum corp    |
| Aes Alamitos Llc                       |  40894 | aesalamitos llc             |
| Aditya Birla Management Corp P         | 190097 | adityabirlamanagementcorpp  |
| Ntpc-Sail Power Co Ltd                 |  16784 | ntpcsailpowerco ltd         |
| Datang Shaanxi Power Generation Co Ltd |  20699 | datangshaanxipowergenco ltd |

``` r
abcd_reduced <- abcd_reduced %>%
  dplyr::mutate(company_alias = to_alias(company_name))

knitr::kable(head(abcd_reduced))
```

|      | company_name                                                 |   id | company_alias                                       |
|:-----|:-------------------------------------------------------------|-----:|:----------------------------------------------------|
| 1    | Berkshire Hathaway, Inc.                                     |  919 | berkshirehathaway inc                               |
| 121  | Ford Motor Co.                                               | 1126 | fordmotor co                                        |
| 481  | Honda Motor Co., Ltd.                                        | 1225 | hondamotorco ltd                                    |
| 817  | Guangzhou State-Owned Assets Supervision & Admin. Commission | 1642 | guangzhoustateownedassetssupervisionadmincommission |
| 937  | China National Machinery Industry Corp.                      | 3140 | chinanationalmachineryindustry corp                 |
| 1033 | Sime Darby Bhd.                                              | 4891 | simedarby bhd                                       |

\#9. matching over company_id: 27715 companies

``` r
loanbook_with_candidates <- financial_data_reduced %>%
  dplyr::inner_join(abcd_reduced, by = c("id"), suffix = c("", "_abcd"))

# i get less candidates if i try to match over company_alias 25085
#loanbook_with_candidates <- financial_data_reduced %>%
#dplyr::inner_join(abcd_reduced, by = c("company_alias"), suffix = c("", "_abcd"))
```

``` r
loanbook_with_candidates_and_dist <- loanbook_with_candidates %>%
  dplyr::mutate(string_sim = stringdist::stringsim(a = .data$company_alias, b = .data$company_alias_abcd, method = "jw", p = 0.1)) %>%
  dplyr::arrange(id, -string_sim)

#knitr::kable(loanbook_with_candidates_and_dist)
```

\#10: filtering out some companies which are under the threshold: 27153

\#11: these lost companies are: 562

``` r
before_filter_id_and_company <- loanbook_with_candidates_and_dist %>% 
  dplyr::select(id, company_name) %>% 
  dplyr::distinct_all()

after_filter_id_and_company <- loanbook_with_candidates_and_dist_filtered %>% 
  dplyr::select(id, company_name) %>% 
  dplyr::distinct_all()

lost_companies <- before_filter_id_and_company %>% 
  dplyr::anti_join(after_filter_id_and_company)
```

    ## Joining with `by = join_by(id, company_name)`

``` r
#knitr::kable(lost_companies)
```

``` r
highest_matches_per_company <- loanbook_with_candidates_and_dist_filtered %>%
  dplyr::group_by(id) %>%
  dplyr::filter(string_sim == max(string_sim))

threshold <- 0.9 # Threshold decided upon extensive experience with r2dii.match function and processes

highest_matches_per_company_above_thresh <- highest_matches_per_company %>%
  dplyr::filter(string_sim > threshold)

highest_matches_per_company_above_thresh_wo_duplicates <- highest_matches_per_company_above_thresh %>%
  dplyr::mutate(duplicates = any(duplicated(company_name))) %>%
  dplyr::filter(duplicates == FALSE) %>%
  dplyr::select(id) %>%
  dplyr::mutate(suggest_match = TRUE)

loanbook_with_candidates_and_dist_and_suggestion <- loanbook_with_candidates_and_dist_filtered %>%
  dplyr::left_join(highest_matches_per_company_above_thresh_wo_duplicates, by = c("id")) %>%
  dplyr::mutate(accept_match = NA)

#knitr::kable(loanbook_with_candidates_and_dist_and_suggestion)
```

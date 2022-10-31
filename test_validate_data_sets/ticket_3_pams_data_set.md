ticket 3 - validate the pams dataset
================
2022-09-16

## task 1: validate that company_id in banks is also part of pams

``` r
original_vs_banks
```

    ##  [1] 1707920  534765  528735  243911  534870  534917  534922  534924  534925
    ## [10]  534929  534930  534936  259266  535020  535073  283395  289477  291699
    ## [19]  535291  639118  260438  634635  535468  535370  535402  304657  535511
    ## [28]  535512  159726   58446  535710  535711  535736  535742  535720  535724
    ## [37]  535725  535727  535728  535730  535731  535729  535738  535739  535767
    ## [46]  724261   61658  535935  535937  535936  535938  535939  361425   41004
    ## [55]   66434  536100  536141  536297  536419   90100  536473  536474  405674
    ## [64]  406393  536621  536693  536694  536695  536696  536701  536702  536738
    ## [73]  536745  536772  536774  536773  536781  536798  536799  536829  536822
    ## [82]  536827  536828  536838  536848

*result: there is a list of company_id that are in banks but not in pams
(85 obs found)*

## task 2: validate the processed pams data

``` r
abcd_stress_test_input <- read_csv("~/Documents/DATA/abcd_stress_test_input.csv")
```

    ## Rows: 5596200 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): company_name, scenario_geography, ald_sector, technology
    ## dbl (5): id, year, plan_tech_prod, plan_emission_factor, plan_sec_prod
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### 2.1. Check for missings values

``` r
report_missings(abcd_stress_test_input, "abcd_stress_test_input.csv", throw_error = TRUE)
```

    ## Reporting missings on dataset: abcd_stress_test_input.csv 
    ## Counted 0 missings on column id 
    ## Counted 0 missings on column company_name 
    ## Counted 0 missings on column scenario_geography 
    ## Counted 0 missings on column year 
    ## Counted 0 missings on column ald_sector 
    ## Counted 0 missings on column technology 
    ## Counted 0 missings on column plan_tech_prod 
    ## Counted 28106 missings on column plan_emission_factor 
    ## Counted 0 missings on column plan_sec_prod

    ## Error: Missings detected on abcd_stress_test_input.csv, please check dataset.

*result: all missings are for plan_emission_factor*

    ##                   id         company_name   scenario_geography 
    ##                  549                  549                   18 
    ##                 year           ald_sector           technology 
    ##                    6                    1                    4 
    ##       plan_tech_prod plan_emission_factor        plan_sec_prod 
    ##                 6743                    1                 4611

    ## 
    ##   HDV 
    ## 28106

*result: all missings have the same ald_sector in common, i.e. HDV*

### 2.2. Check for duplicates

``` r
report_duplicates(abcd_stress_test_input, c("id", "company_name", "scenario_geography", "year", "ald_sector", "technology"), throw_error = TRUE)
```

*result: does not throw error - no duplicates found*

## task 3: How many rows are gained by using processed pams compared to banks

``` r
dim(Access_Bank_2022_04_07_AR_2021Q4_Free_Dataset_PACTA_for_Banks_Equity_Ownership_Consolidation)
```

    ## [1] 444094     13

``` r
dim(abcd_stress_test_input)
```

    ## [1] 5596200       9

*result: 5152106 observation in addition; or should we compare to PACTA
?*

ticket_4\_compare_company_names financial v abcd
================
2022-09-28

\#1.Count uniqe company_name in abcd_stress_test (42 500)

``` r
dim(abcd_full_names)
```

    ## [1] 42500     1

\#2.Count uniqe company_name in financial_data (50 492)

``` r
dim(financial_data_names)
```

    ## [1] 50492     1

\#3.Match company names

``` r
check <- dplyr::inner_join(abcd_full_names,financial_data_names, by = "company_name")

dim(check)
```

    ## [1] 12859     1

*It seems that mostly matching does not work bc company names slightly
differ* *e.g. Solvay SA (abcd_stress_test) is Solvay Sa (financial
data)* *e.g.Canada Pension Plan Investment Board is Canada Pension Plan
Investment*

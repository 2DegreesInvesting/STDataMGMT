
<!-- README.md is generated from README.Rmd. Please edit that file -->

# STDataMGMT

<!-- badges: start -->
<!-- badges: end -->

The goal of STDataMGMT is to provide the input data required for the
stress test that cannot be shared as part of a public repository. The
2DII stress test needs several external data inputs that usually require
a license which prohibits redistributing the data. 2DII does not have
the right to publicly share that data though, which is why this
repository needs to stay private.The repository will be used to point
the main stress testing code to, when said data needs to be loaded. This
allows publishing the code containing 2DII’s main climate stress testing
methodology open source.

The underlying logic of the data preparation is the following quintet:

1.  raw data are stored with the affix **raw\_** in the data-raw folder

2.  in the R folder the data preparation function is stored with the
    affix **prepare\_/aggregate\_**

3.  in the data-raw folder the file that calls the data preparation
    function is stored with the affix **run\_**

4.  prepared data are stored in the data-raw folder without affix

5.  in the test folder the test of the data preparation function is
    stored with the affix **test\_**

## prepare_data.R - Functions for data preparation

### Preparation of scenario data

In order to get the scenario data into the required format as defined in
`r2dii.climate.stress.test`, the function `prepare_scenario_data()`
assumes that input in the format of data found on the dropbox under
***Processed Data*** is available. The input argument should point to a
corresponding file. The start year of the scenario file must be defined,
as does the output directory (NOT the file name). Please do not write
the output data to any directory in this repository, unless you are
specifically tasked with updating the default auxiliary files for the
2DII stress test.

**ADD INFO ON PACTA ROUTINE ONCE CLEAR**

### Preparation of production data

Production data preparation uses the PAMS dataset from Asset Resolution
as an input, which is stored on the dropbox under
***ST_INPUTS_PRODUCTION***. Start year, as well as time-horizon,
scenario sources, and sectors are defined manually.Data are aggregated
to company, technology, geography and year level and production as well
as emission factors are retained.

We remove entries with faulty or missing ownership information and
sectors that we cannot cover in the stress test as of now. Entries with
missing information on ALD locations are removed as well.For all entries
that have missing emission factors, we mean impute based on the
corresponding combination of country, technology, year or sub region,
technology, year.

### Preparation of capacity data

The function `prepare_prewrangled_capacity_factors_WEOYYYY()` reads in
raw power generation and capacity data and wrangles that data into the
required format for usage in `r2dii.climate.stress.test`. It uses input
files that are to be found on the dropbpox under **Raw_Data**, under
`WEOYYYY_Raw_Data.csv`, with YYYY being the year of the release of the
report.The user furthermore has to set a start year and an end year of
the preparation time frame. The start year should correspond with the
year of publication and the end year should be the same as the one used
in `r2dii.climate.stress.test`.

Beside wrangling the data into the expected format in terms of pivoting
and renaming columns, the function will linearly interpolate missing
values between the start and end years of the preparation time frame for
power generation and capacity. It will transform both indicators into
the same unit (GW) and calculate capacity factors per scenario, scenario
geography, sector, technology and year. It also ensures the
sector/technology breakdown is the same as in
`r2dii.climate.stess.test`, as long as the input file is an IEA WEO
release.

Lastly, the directory to which the resulting file will be written has to
be set. By default, the function will not overwrite any file within this
repository. So if you are trying to update the contents of the
repository, you will have to do that explicitly.

### Preparation of price data

The function `prepare_price_data_long_WEOYYYY()` reads price data based
on the raw data format provided by IEA WEO and wrangles that data into
clean long format.The start year has to be set manually. For fossil
fuels, price data by scenario is derived from **Ask Antonio**. For power
generation, data on the levelized cost of electricity (LCOE) by scenario
is taken from
<https://www.iea.org/articles/levelised-cost-of-electricity-calculator>
**Ask Antonio**. The function `prepare_lcoe_adjusted_price_data_weo()`
reads in the long format prepared price data from IEA scenarios and
filters LCOE for the power sector. Using the LCOE for power generation
implies that this indicator must be transformed into effective prices
for each company, by applying the following transformation:

-   LCOE = (1 - net_profit_margin) \* price
-   price = LCOE / (1 - net_profit_margin)

This transformation happens in `r2dii.cliamte.stress.test` though, as it
requires company level information.

We want to be able to use all data at least on a global granularity,
with additional layers of granularity being a nice-to-have. For some
variables, the raw data only contain a list of selected regional data.
In these cases, we approximate the global price as a simple arithmetic
mean of the regional prices. A more sophisticated approximation would
probably take into account the sizes of the markets and add some missing
regions, but for our purposes, this is still a reasonable approximation.
Specifically, we use this approximation for Coal and Gas extraction as
well as for all given types of power generation.

### Preparation of financial data

In order to obtain all relevant types of financial information for the
companies for which we have production data (abcd_stress_test_input), we
need to merge and clean the raw financial data from Eikon with the
master data files provided by Asset Resolution.

The raw Eikon data files are stored as xlsx files on the dropbox under
**FinancialData**.The asset resolution and 2DII input files are sourced
from data store and the analysis input directories from the dropbox.

Data Store:

company_ownership_bidirectional.csv masterdata_credit_methodology.csv

Analysis inputs:

security_financial_data.rda consolidated_financial_data.rda
masterdata_ownership_datastore.rda masterdata_debt_datastore.rda

`prepare_eikon_data()` reads and cleans the financial data sourced from
Eikon. Missing and duplicate ISINs from Eikon are removed and the
remaining data is inner joined on the security financial data, so that
only entries remain that we can identify in our stress testing
analyses.Subsequently the ownership structure is merged to the data
using the ownership tree and only that entry per company is retained
that is closest to the ultimate parent. The next step is to add all
companies for which we have production data again, that could not
directly be matched between abcd_stress_test_input and Eikon. This will
later get augmented with averages.

# EXPORTED DATASETS ========================================

#' master Scenarios_AnalysisInput used in the stress test
#' @source see generative script in tests folder
"Scenarios_AnalysisInput"

#' master price_data_long used in the stress test
#' @source see generative script in tests folder
"price_data_long"

#' master prewrangled_capacity_factors used in the stress test
#' @source see generative script in tests folder
"prewrangled_capacity_factors"

#' master ngfs_carbon_price used in the stress test
#' @source see generative script in tests folder
"ngfs_carbon_price"

# PREPROCESSING DATASETS ========================================

#' available geographies in the stress test
#'
#' @source internal
"scenarios_geographies"


# MOCK DATASETS ========================================

#' Used to generate company synthetic datasets
#' TODO remove to be 
#' @source Scenario analysis input available production types
"production_types"


#' GENERATE COMPANY ACTIVITY
#' @source see generative script in tests folder
"synthetic_company_activities"

#' GENERATE COMPANY EMISSIONS
#' generated from company_activities
#' @source see generative script in tests folder
"synthetic_company_emissions"


#' generated
#' @source see generative script in tests folder
"synthetic_eikon_data"
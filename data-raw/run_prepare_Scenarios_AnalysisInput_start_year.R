devtools::load_all()

#set start_year
start_year_despite_old_data <- start_year   # defined in workflow.R
start_year <- 2021 # TODO FIX with start_year // remove start_year from files names

#vector of low carbon technologies 
green_techs <- c(
  "FuelCell",
  "Electric",
  "Hybrid",
  "RenewablesCap",
  "HydroCap",
  "NuclearCap",
  "FuelCell_HDV",
  "Electric_HDV",
  "Hybrid_HDV"
)

# scenario values will be linearly interpolated for each group below
interpolation_groups <- c(
  "source",
  "scenario",
  "sector",
  "technology",
  "scenario_geography",
  "indicator",
  "units"
)


#WEO data from PACTA routine
 input_path <- fs::path(
   "data-raw",
   "scenario_analysis_input_data",
   glue::glue("weo_Scenarios_AnalysisInput_{start_year}.csv")
 )

 weo_data <- readr::read_csv(
   input_path,
   col_types = readr::cols_only(
     source = "c",
     scenario = "c",
     scenario_geography = "c",
     sector = "c",
     technology = "c",
     units = "c",
     indicator = "c",
     year = "d",
     value = "d"
   )
 )
 
#GECO data from PACTA routine 
 input_path <- fs::path(
      "data-raw",
   "scenario_analysis_input_data",
   glue::glue("pacta_processed_geco_Scenarios_AnalysisInput_{start_year}.csv")
 )
 
 geco_data <- readr::read_csv(
   input_path,
   col_types = readr::cols_only(
     source = "c",
     scenario = "c",
     scenario_geography = "c",
     sector = "c",
     technology = "c",
     units = "c",
     indicator = "c",
     year = "d",
     value = "d"
   )
 ) 
 
# combine WEO with GECO data 
 weo_geco_data <- rbind(
   weo_data,
   geco_data
 )
 
 weo_geco_data <- weo_geco_data %>%
   interpolate_yearly(!!!rlang::syms(interpolation_groups)) %>%
   dplyr::filter(year >= start_year_despite_old_data) %>% 
   add_market_share_columns(start_year_despite_old_data = start_year_despite_old_data) 
 
 weo_geco_data <- weo_geco_data %>%
   format_p4i(green_techs)
 
prepared_data <- prepare_scenario_data(data = weo_geco_data)


#NGFS 
input_path <- fs::path(
     "data-raw",
   "scenario_analysis_input_data",
  glue::glue("ngfs_Scenarios_AnalysisInput_{start_year}.csv")
)

ngfs_data <- readr::read_csv(
  input_path,
  col_types = readr::cols_only(
    Model = "c",
    Scenario = "c",
    Region = "c",
    Variable = "c",
    category_a = "c",
    category_b = "c",
    category_c = "c",
    Unit = "c",
    year = "d",
    value = "d"
  )
)

preprepared_ngfs_data <- preprepare_ngfs_scenario_data(ngfs_data, 
                                                       start_year = start_year)


preprepared_ngfs_data <- preprepared_ngfs_data %>%
  interpolate_yearly(!!!rlang::syms(interpolation_groups)) %>%
  dplyr::filter(year >= start_year_despite_old_data) %>%
  add_market_share_columns(start_year_despite_old_data = start_year_despite_old_data) 

preprepared_ngfs_data <- preprepared_ngfs_data %>% format_p4i(green_techs)

preprepared_ngfs_data <- style_ngfs(preprepared_ngfs_data)

# replace nan fair_share_perc by 0. Nans appear when dividing per 0 in the tmsr computation
preprepared_ngfs_data <- preprepared_ngfs_data %>%
  dplyr::mutate(fair_share_perc = dplyr::if_else(is.na(fair_share_perc), 0, fair_share_perc))

### IPR Scenario
### Read IPR

input_path <- fs::path(
  "data-raw",
   "scenario_analysis_input_data",
  glue::glue("ipr_Scenarios_AnalysisInput_{start_year}.csv")
)

IPR <- as.data.frame(readr::read_csv(
  input_path,
  col_types = readr::cols_only(
    Scenario = "c",
    Region = "c",
    Sector = "c",
    Units = "c",
    Variable_class = "c",
    Sub_variable_class_1 = "c",
    Sub_variable_class_2 = "c",
    year = "d",
    value = "d"
  )
))

prepared_IPR_data <- prepare_IPR_scenario_data(IPR, 
                                               start_year_despite_old_data = start_year_despite_old_data)
# IPR baseline scenario
# IPR baseline is a duplicate of the WEO2021 STEPs scenario

IPR_baseline <- prepare_IPR_baseline_scenario(prepared_data)

# joining IPR scenarios

prepared_IPR_data <- dplyr::full_join(prepared_IPR_data, IPR_baseline)

# replace nan fair_share_perc by 0. Nans appear when dividing per 0 in the tmsr computation
prepared_IPR_data <- prepared_IPR_data %>%
  dplyr::mutate(fair_share_perc = dplyr::if_else(is.na(fair_share_perc), 0, fair_share_perc))

### Oxford Scenario
### Read Oxford

input_path <- fs::path(
  "data-raw",
   "scenario_analysis_input_data",
  glue::glue("oxford_Scenarios_AnalysisInput_{start_year}.csv")
)

OXF <- as.data.frame(readr::read_csv(
  input_path,
  col_types = readr::cols_only(
    "Annual energy" = "c",
    units = "c",
    scenario = "c",
    scenario_geography = "c",
    year = "d",
    value = "d"
  )
))
prepared_OXF_data <- prepare_OXF_scenario_data(OXF, 
                                               start_year_despite_old_data = start_year_despite_old_data)

### Merge Data from Scenario Sources
prepared_data_IEA_NGFS <- dplyr::full_join(prepared_data, preprepared_ngfs_data)
prepared_data_IPR_OXF <- dplyr::full_join(prepared_IPR_data, prepared_OXF_data)
prepared_data_combined <- dplyr::full_join(prepared_data_IEA_NGFS, prepared_data_IPR_OXF)

prepared_data_combined %>% 
  dplyr::rename(ald_business_unit=.data$technology) %>%
  readr::write_csv(
  file.path("data-raw", "st_inputs",glue::glue("Scenarios_AnalysisInput_{start_year}.csv"))
)

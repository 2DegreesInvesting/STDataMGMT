devtools::load_all()

##ok. we need to add weo_2021 manually that is the output of the PACTA routine with start_year == 2021 
##it is not pretty but no other way around 

scenario_input_p4i_weo2021 <- read.csv("~/Dropbox (2° Investing)/PortCheck/00_Data/01_ProcessedData/03_ScenarioData/scenario_input_p4i_weo2021.csv")

Scenarios_AnalysisInput_2021 <- read.csv("~/Dropbox (2° Investing)/PortCheck/00_Data/01_ProcessedData/03_ScenarioData/Scenarios_AnalysisInput_2021.csv")

weo2021_manually_added_Scenarios_AnalysisInput_2021 <- rbind(scenario_input_p4i_weo2021, Scenarios_AnalysisInput_2021)

weo2021_manually_added_Scenarios_AnalysisInput_2021$ScenarioGeography <- str_replace(weo2021_manually_added_Scenarios_AnalysisInput_2021$ScenarioGeography, "Central&SouthAmerica", "CentralandSouthAmerica")


write.csv(weo2021_manually_added_Scenarios_AnalysisInput_2021,
          "~/Dropbox (2° Investing)/PortCheck/00_Data/01_ProcessedData/03_ScenarioData/weo2021_manually_added_Scenarios_AnalysisInput_2021.csv", row.names = FALSE)



# make sure to set the relevant start year when running the data preparation
# example:
start_year <- 2021
input_path <- r2dii.utils::path_dropbox_2dii(
  "PortCheck",
  "00_Data",
  "01_ProcessedData",
  "03_ScenarioData",
  glue::glue("weo2021_manually_added_Scenarios_AnalysisInput_{start_year}.csv")
)

input_data <- readr::read_csv(
  input_path,
  col_types = readr::cols_only(
    Source = "c",
    Technology = "c",
    ScenarioGeography = "c",
    Sector = "c",
    Units = "c",
    Indicator = "c",
    Scenario = "c",
    Sub_Technology = "c",
    Year = "d",
    AnnualvalIEAtech = "d",
    refvalIEAtech = "d",
    refvalIEAsec = "d",
    Direction = "c",
    mktFSRatio = "d",
    techFSRatio = "d",
    FairSharePerc = "d"
  )
)

prepared_data <- prepare_scenario_data(data = input_data, start_year = start_year)

# due to likely manual errors or alike developing economies and advanced
# economies have different name formatting from different sectors in 2020 data.
# Fixing this here in a hardcoded way.
if (start_year == 2020) {
  prepared_data <- prepared_data %>%
    dplyr::mutate(
      scenario_geography = dplyr::if_else(
        .data$scenario_geography == "Advancedeconomies",
        "AdvancedEconomies",
        .data$scenario_geography
      )
    ) %>%
    dplyr::mutate(
      scenario_geography = dplyr::if_else(
        .data$scenario_geography == "Developingeconomies",
        "DevelopingEconomies",
        .data$scenario_geography
      )
    )
}
#EU27 and EU are two distinct in the 2021 data which are combined here 
#same goes for advanced - and emerging market and developing economies
if (start_year == 2021) {
  prepared_data <- prepared_data %>%
    dplyr::mutate(
      scenario_geography = dplyr::if_else(
        .data$scenario_geography == "Advanced economies",
        "AdvancedEconomies",
        .data$scenario_geography
      )
    ) %>%
    dplyr::mutate(
      scenario_geography = dplyr::if_else(
        .data$scenario_geography == "Emerging market and developing economies",
        "Emergingmarket&developingeconomies",
        .data$scenario_geography
      )
    ) %>%
    dplyr::mutate(
      scenario_geography = dplyr::if_else(
        .data$scenario_geography == "EU27",
        "EU",
        .data$scenario_geography
      )
    )
}



prepared_data %>% readr::write_csv(
  file.path("data-raw", glue::glue("weo2021_manually_added_Scenarios_AnalysisInput_{start_year}.csv"))
)

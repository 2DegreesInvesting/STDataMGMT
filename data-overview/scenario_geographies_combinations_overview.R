library(tidyverse)
library(datapasta)
devtools::load_all()

# Overview of supported scenarios x scenario-geographies x sectors --------
# Scenario_AnalysisInput_2021 ---------------------------------------------
Scenario_AnalysisInput_2021 <- readr::read_csv(
  file.path("data-raw", glue::glue("weo2021_manually_added_Scenarios_AnalysisInput_2021.csv"))
)

# we can only include geographies that are present both in baseline (currently APS, SPS, GEO ref )
# and shock scenario (currently SDS, nze_2050)
Scenario_AnalysisInput_2021_STEPS <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("WEO2021_STEPS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_SDS <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("WEO2021_SDS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_APS <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("WEO2021_APS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

# this only has 4 scenario geographies
Scenario_AnalysisInput_2021_NZE_2050 <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("WEO2021_NZE_2050")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

#GECO baseline (_ref) and shock (_1.5c, 2c_m)
Scenario_AnalysisInput_2021_GECO2019_ref <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("GECO2019_ref")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_GECO2019_1.5c<- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("GECO2019_1.5c")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_GECO2019_2c_m<- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("GECO2019_2c_m")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_without_nze  <- Scenario_AnalysisInput_2021_APS %>%
  select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_SDS %>%
               select(scenario_geography, ald_sector) %>%
 inner_join(Scenario_AnalysisInput_2021_STEPS) %>%
               select(scenario_geography, ald_sector)) %>%
  arrange(scenario_geography)

#Scenario_AnalysisInput_2021_without_nze <- Scenario_AnalysisInput_2021_without_nze %>% tribble_paste()
tibble::tribble(
                         ~scenario_geography, ~ald_sector,
                        "Advanced economies",      "Coal",
                        "Advanced economies",   "Oil&Gas",
                        "Advanced economies",     "Power",
                                    "Africa",      "Coal",
                                    "Africa",   "Oil&Gas",
                                    "Africa",     "Power",
                               "AsiaPacific",      "Coal",
                               "AsiaPacific",   "Oil&Gas",
                               "AsiaPacific",     "Power",
                                    "Brazil",     "Power",
                                     "China",     "Power",
  "Emerging market and developing economies",      "Coal",
  "Emerging market and developing economies",   "Oil&Gas",
  "Emerging market and developing economies",     "Power",
                                      "EU27",     "Power",
                                   "Eurasia",      "Coal",
                                   "Eurasia",   "Oil&Gas",
                                   "Eurasia",     "Power",
                                    "Europe",      "Coal",
                                    "Europe",   "Oil&Gas",
                                    "Europe",     "Power",
                                    "Global",      "Coal",
                                    "Global",   "Oil&Gas",
                                    "Global",     "Power",
                                     "India",     "Power",
                                     "Japan",     "Power",
                              "LatinAmerica",      "Coal",
                              "LatinAmerica",   "Oil&Gas",
                              "LatinAmerica",     "Power",
                                "MiddleEast",      "Coal",
                                "MiddleEast",   "Oil&Gas",
                                "MiddleEast",     "Power",
                                   "NonOECD",      "Coal",
                                   "NonOECD",   "Oil&Gas",
                                   "NonOECD",     "Power",
                              "NorthAmerica",      "Coal",
                              "NorthAmerica",   "Oil&Gas",
                              "NorthAmerica",     "Power",
                                      "OECD",      "Coal",
                                      "OECD",   "Oil&Gas",
                                      "OECD",     "Power",
                                    "Russia",     "Power",
                                        "US",     "Power"
  )


Scenario_AnalysisInput_2021_scenarios  <- Scenario_AnalysisInput_2021_APS %>%
  select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_SDS) %>%
  select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_NZE_2050) %>%
  select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_STEPS) %>%
  select(scenario_geography, ald_sector) %>%
  arrange(scenario_geography)

#Scenario_AnalysisInput_2021_scenarios <- Scenario_AnalysisInput_2021_scenarios %>% %>% tribble_paste()
tibble::tribble(
  ~scenario_geography, ~ald_sector,
             "Global",      "Coal",
             "Global",   "Oil&Gas",
             "Global",     "Power"
  )


# prewrangled_capacity_factors --------------------------------------------
# NOTE: Only relevant for power sector

# prewrangled_capacity_factors
#using prepared_data as prewrangled_capacity_data is outdated address!-----------------------------------
prewrangled_capacity_factors <- readr::read_csv(
  file.path("data-raw", glue::glue("prewrangled_capacity_factors.csv"))
)

# we can only include geographies that are present both in baseline (currently NPS)
# and shock scenario (currently SDS)
prewrangled_capacity_factors_WEO2021_STEPS <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("WEO2021_STEPS")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_WEO2021_SDS <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("WEO2021_SDS")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_WEO2021_APS <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("WEO2021_APS")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_WEO2021_NZE_2050 <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("WEO2021_NZE_2050")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()


prewrangled_capacity_factors_WEO_2021_scenarios <- prewrangled_capacity_factors_WEO2021_STEPS %>%
  select(scenario_geography) %>%
  inner_join(prewrangled_capacity_factors_WEO2021_SDS %>%
               select(scenario_geography)) %>%
  inner_join(prewrangled_capacity_factors_WEO2021_APS %>%
               select(scenario_geography)) %>%
  inner_join(prewrangled_capacity_factors_WEO2021_NZE_2050 %>%
               select(scenario_geography)) %>%
  arrange(scenario_geography)

# prewrangled_capacity_factors_WEO_2021_scenarios %>% tribble_paste()
tibble::tribble(
          ~scenario_geography,
         "Advanced Economies",
                     "Africa",
               "Asia Pacific",
                     "Brazil",
  "Central and South America",
                      "China",
       "Developing Economies",
                    "Eurasia",
                     "Europe",
             "European Union",
                     "Global",
                      "India",
                      "Japan",
                "Middle East",
                   "Non-OECD",
              "North America",
                       "OECD",
                     "Russia",
               "South Africa",
             "Southeast Asia",
              "United States"
  )


# Supported overlap -------------------------------------------------------
# We can only offer scenario_geography x region combinations for which baseline and
# shock results are available in all relevant dataset.
# This overlap is pointed here for datasets as currently used for ST user workflow

# Harmonizing naming conventions to P4I standard
cap_fac_harmonised <- prewrangled_capacity_factors_WEO_2021_scenarios %>%
  dplyr::mutate(scenario_geography = gsub(" ", "", scenario_geography, fixed = TRUE)) %>%
  dplyr::mutate(scenario_geography = case_when(scenario_geography == "EuropeanUnion" ~ "EU",
                                               scenario_geography == "Non-OECD" ~ "NonOECD",
                                               scenario_geography == "UnitedStates" ~ "US",
                                               TRUE ~ scenario_geography))

Scenario_AnalysisInput_2021_without_nze_harmonized <- Scenario_AnalysisInput_2021_without_nze %>%
  dplyr::mutate(scenario_geography = case_when(scenario_geography == "EU27" ~ "EU",
                                               scenario_geography == "Emerging market and developing economies"~ "Emergingmarket&developingeconomies",
                                               scenario_geography == "Advanced economies"~  "AdvancedEconomies",
                                                TRUE ~ scenario_geography))


# removing scenario_geography x Power combinations that are missing in cap_fac_harmonised
overlap_all <- Scenario_AnalysisInput_2021_without_nze_harmonized  %>%
  filter(!(ald_sector == "Power" & !.data$scenario_geography %in% cap_fac_harmonised$scenario_geography))


#overlap_all %>% tribble_paste()
tibble::tribble(
                         ~scenario_geography, ~ald_sector,
                        "Advanced economies",      "Coal",
                        "Advanced economies",   "Oil&Gas",
                                    "Africa",      "Coal",
                                    "Africa",   "Oil&Gas",
                                    "Africa",     "Power",
                               "AsiaPacific",      "Coal",
                               "AsiaPacific",   "Oil&Gas",
                               "AsiaPacific",     "Power",
                                    "Brazil",     "Power",
                                     "China",     "Power",
  "Emerging market and developing economies",      "Coal",
  "Emerging market and developing economies",   "Oil&Gas",
                                   "Eurasia",      "Coal",
                                   "Eurasia",   "Oil&Gas",
                                   "Eurasia",     "Power",
                                    "Europe",      "Coal",
                                    "Europe",   "Oil&Gas",
                                    "Europe",     "Power",
                                    "Global",      "Coal",
                                    "Global",   "Oil&Gas",
                                    "Global",     "Power",
                                     "India",     "Power",
                                     "Japan",     "Power",
                              "LatinAmerica",      "Coal",
                              "LatinAmerica",   "Oil&Gas",
                                "MiddleEast",      "Coal",
                                "MiddleEast",   "Oil&Gas",
                                "MiddleEast",     "Power",
                                   "NonOECD",      "Coal",
                                   "NonOECD",   "Oil&Gas",
                                   "NonOECD",     "Power",
                              "NorthAmerica",      "Coal",
                              "NorthAmerica",   "Oil&Gas",
                              "NorthAmerica",     "Power",
                                      "OECD",      "Coal",
                                      "OECD",   "Oil&Gas",
                                      "OECD",     "Power",
                                    "Russia",     "Power",
                                        "US",     "Power"
  )



##BEFORE
# overlap_all %>% tribble_paste()
# tibble::tribble(
#   ~scenario_geography,  ~ald_sector,
#   "AdvancedEconomies",       "Coal",
#   "AdvancedEconomies",    "Oil&Gas",
#   "AdvancedEconomies",      "Power",
#   "Africa",      "Power",
#   "Africa",       "Coal",
#   "Africa",    "Oil&Gas",
#   "AsiaPacific",      "Power",
#   "AsiaPacific",       "Coal",
#   "AsiaPacific",    "Oil&Gas",
#   "Brazil",      "Power",
#   "CentralandSouthAmerica",      "Power",
#   "CentralandSouthAmerica",       "Coal",
#   "CentralandSouthAmerica",    "Oil&Gas",
#   "China",      "Power",
#   "DevelopingEconomies",       "Coal",
#   "DevelopingEconomies",    "Oil&Gas",
#   "DevelopingEconomies",      "Power",
#   "EU",      "Power",
#   "EU",    "Oil&Gas",
#   "Eurasia",      "Power",
#   "Eurasia",       "Coal",
#   "Eurasia",    "Oil&Gas",
#   "Europe",      "Power",
#   "Europe",       "Coal",
#   "Europe",    "Oil&Gas",
#   "Global", "Automotive",
#   "Global",      "Power",
#   "Global",       "Coal",
#   "Global",    "Oil&Gas",
#   "India",      "Power",
#   "Japan",      "Power",
#   "MiddleEast",      "Power",
#   "MiddleEast",       "Coal",
#   "MiddleEast",    "Oil&Gas",
#   "NonOECD",      "Power",
#   "NonOECD",       "Coal",
#   "NonOECD",    "Oil&Gas",
#   "NorthAmerica",      "Power",
#   "NorthAmerica",       "Coal",
#   "NorthAmerica",    "Oil&Gas",
#   "OECD",      "Power",
#   "OECD",       "Coal",
#   "OECD",    "Oil&Gas",
#   "Russia",      "Power",
#   "SouthAfrica",      "Power",
#   "US",      "Power"
# )

c("AdvancedEconomies", "Africa", "AsiaPacific", "Brazil", "China", "Emergingmarket&developingeconomies", "EU", "Eurasia", "Europe", "Global", "India", "Japan", "LatinAmerica", "MiddleEast", "NonOECD", "NorthAmerica", "OECD", "Russia", "US")


library(tidyverse)
library(datapasta)
devtools::load_all()

# This document is a preparation for the whitelisting of geography x
# sector x scenario combinations in the climate.stress.test.repo

# Overview of supported scenarios x scenario-geographies x sectors
# Scenario_AnalysisInput_2022
Scenario_AnalysisInput_2022 <- readr::read_csv(
  file.path("data-raw", glue::glue("Scenarios_AnalysisInput_2021.csv"))
)

# first look at whether the sectors are complete
# if active in power, oil&gas and coal -> 9 technologies
# if active in  oil&gas and coal -> 3 technologies
# if active in  oil&gas and power -> 8 technologies
# if active only in power -> 6 technologies
# if active only in coal -> 1 technology
# if active only in oil&gas -> 2 technologies
Scenario_AnalysisInput_2022 %>%
  group_by(scenario_geography, scenario) %>%
  summarise(
    nrow = n(),
    n_sector = length(unique(ald_sector)),
    n_technologies = length(unique(technology)),
    sectors = list(unique(ald_sector)),
    technologies = list(unique(technology))
  ) %>%
  dplyr::arrange(scenario_geography) %>%
  View()

# remove sector if not complete
p4i_p4b_sector_technology_lookup_df <- p4i_p4b_sector_technology_lookup()

Scenario_AnalysisInput_2022 <- Scenario_AnalysisInput_2022 %>%
  dplyr::filter(Scenario_AnalysisInput_2022$ald_sector %in% unique(p4i_p4b_sector_technology_lookup_df$sector_p4i))

Scenario_AnalysisInput_2022 <- remove_incomplete_sectors(Scenario_AnalysisInput_2022)

###############################################################################
# WEO scenarios
# we have 19 scenario geographies (12 with all sectors and 7 only for power)
Scenario_AnalysisInput_2022_STEPS <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("WEO2021_STEPS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_SDS <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("WEO2021_SDS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_APS <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("WEO2021_APS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

# this only has 4 scenario geographies
Scenario_AnalysisInput_2022_NZE_2050 <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("WEO2021_NZE_2050")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

# joining WEO scenario data for the scenarios with all scenario geographies
Scenario_AnalysisInput_2022_WEO_without_NZE_2050 <- Scenario_AnalysisInput_2022_APS %>%
  select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2022_SDS) %>%
  select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2022_STEPS) %>%
  select(scenario_geography, ald_sector) %>%
  arrange(scenario_geography)
  
# Scenario_AnalysisInput_2022_WEO_without_NZE_2050 <- Scenario_AnalysisInput_2022_WEO_without_NZE_2050 %>% tribble_paste()
tibble::tribble(
                     ~scenario_geography, ~ald_sector,
                     "AdvancedEconomies",      "Coal",
                     "AdvancedEconomies",   "Oil&Gas",
                     "AdvancedEconomies",     "Power",
                                "Africa",      "Coal",
                                "Africa",   "Oil&Gas",
                                "Africa",     "Power",
                           "AsiaPacific",      "Coal",
                           "AsiaPacific",   "Oil&Gas",
                           "AsiaPacific",     "Power",
                                "Brazil",     "Power",
                                 "China",     "Power",
                                  "EU27",     "Power",
  "EmergingMarketAndDevelopingEconomies",      "Coal",
  "EmergingMarketAndDevelopingEconomies",   "Oil&Gas",
  "EmergingMarketAndDevelopingEconomies",     "Power",
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
                              "Non-OECD",      "Coal",
                              "Non-OECD",   "Oil&Gas",
                              "Non-OECD",     "Power",
                          "NorthAmerica",      "Coal",
                          "NorthAmerica",   "Oil&Gas",
                          "NorthAmerica",     "Power",
                                  "OECD",      "Coal",
                                  "OECD",   "Oil&Gas",
                                  "OECD",     "Power",
                                "Russia",     "Power",
                          "UnitedStates",     "Power"
  )

###############################################################################
# GECO scenarios
# we have 1 scenario geographies 
Scenario_AnalysisInput_2022_GECO_CurPol <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("GECO2021_CurPol")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_GECO_NDC_LTS <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("GECO2021_NDC-LTS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_GECO_1.5c <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("GECO2021_1.5C-Unif")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

# binding GECO scenario data
Scenario_AnalysisInput_2022_GECO <- Scenario_AnalysisInput_2022_GECO_CurPol %>%
  full_join(Scenario_AnalysisInput_2022_GECO_NDC_LTS) %>%
              full_join(Scenario_AnalysisInput_2022_GECO_1.5c) %>%
              arrange(scenario_geography)

# Scenario_AnalysisInput_2022_GECO <-  Scenario_AnalysisInput_2022_GECO %>% tribble_paste()
tibble::tribble(
             ~scenario, ~scenario_geography,  ~ald_sector,
     "GECO2021_CurPol",            "Global", "Automotive",
    "GECO2021_NDC-LTS",            "Global", "Automotive",
  "GECO2021_1.5C-Unif",            "Global", "Automotive"
  )

###############################################################################
# NGFS scenarios

#NGFS GCAM 
#11 regions (TO DO: INDIA is missing here)
Scenario_AnalysisInput_2022_GCAM_B2DS <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_GCAM_B2DS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_GCAM_CP <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_GCAM_CP")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_GCAM_DN0 <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_GCAM_DN0")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_GCAM_DT <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_GCAM_DT")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_GCAM_NDC <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_GCAM_NDC")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_GCAM_NZ2050 <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_GCAM_NZ2050")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

#combine GCAM Scenarios 
Scenario_AnalysisInput_2022_NGFS_GCAM <- Scenario_AnalysisInput_2022_GCAM_B2DS %>%
  select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2022_GCAM_CP %>%
               select(scenario_geography, ald_sector) %>%
               inner_join(Scenario_AnalysisInput_2022_GCAM_DN0 %>%
                            select(scenario_geography, ald_sector) %>%
                            inner_join(Scenario_AnalysisInput_2022_GCAM_DT %>%
                                         select(scenario_geography, ald_sector) %>%
                                         inner_join(Scenario_AnalysisInput_2022_GCAM_NDC %>%
                                                      select(scenario_geography, ald_sector) %>%
                                                      inner_join(Scenario_AnalysisInput_2022_GCAM_NZ2050 %>%
                                                                   select(scenario_geography, ald_sector)))))) %>%
  arrange(scenario_geography)

#Scenario_AnalysisInput_2022_NGFS_GCAM <- Scenario_AnalysisInput_2022_NGFS_GCAM %>% tribble_paste()

tibble::tribble(
    ~scenario_geography, ~ald_sector,
                 "Asia",      "Coal",
                 "Asia",   "Oil&Gas",
                 "Asia",     "Power",
                "China",      "Coal",
                "China",   "Oil&Gas",
                "China",     "Power",
               "Global",      "Coal",
               "Global",   "Oil&Gas",
               "Global",     "Power",
            "Indonesia",      "Coal",
            "Indonesia",   "Oil&Gas",
            "Indonesia",     "Power",
                "Japan",      "Coal",
                "Japan",   "Oil&Gas",
                "Japan",     "Power",
         "LatinAmerica",      "Coal",
         "LatinAmerica",   "Oil&Gas",
         "LatinAmerica",     "Power",
  "MiddleEastAndAfrica",      "Coal",
  "MiddleEastAndAfrica",   "Oil&Gas",
  "MiddleEastAndAfrica",     "Power",
            "OecdAndEu",      "Coal",
            "OecdAndEu",   "Oil&Gas",
            "OecdAndEu",     "Power",
   "ReformingEconomies",      "Coal",
   "ReformingEconomies",   "Oil&Gas",
   "ReformingEconomies",     "Power",
        "SoutheastAsia",      "Coal",
        "SoutheastAsia",   "Oil&Gas",
        "SoutheastAsia",     "Power",
         "UnitedStates",      "Coal",
         "UnitedStates",   "Oil&Gas",
         "UnitedStates",     "Power"
  )


#NGFS REMIND 
#10 regions
Scenario_AnalysisInput_2022_REMIND_B2DS <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_REMIND_B2DS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_REMIND_CP <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_REMIND_CP")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_REMIND_DN0 <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_REMIND_DN0")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_REMIND_DT <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_REMIND_DT")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_REMIND_NDC <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_REMIND_NDC")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_REMIND_NZ2050 <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_REMIND_NZ2050")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

#combine REMIND Scenarios 
Scenario_AnalysisInput_2022_NGFS_REMIND <- Scenario_AnalysisInput_2022_REMIND_B2DS %>%
  select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2022_REMIND_CP %>%
               select(scenario_geography, ald_sector) %>%
               inner_join(Scenario_AnalysisInput_2022_REMIND_DN0 %>%
                            select(scenario_geography, ald_sector) %>%
                            inner_join(Scenario_AnalysisInput_2022_REMIND_DT %>%
                                         select(scenario_geography, ald_sector) %>%
                                         inner_join(Scenario_AnalysisInput_2022_REMIND_NDC %>%
                                                      select(scenario_geography, ald_sector) %>%
                                                      inner_join(Scenario_AnalysisInput_2022_REMIND_NZ2050 %>%
                                                                   select(scenario_geography, ald_sector)))))) %>%
  arrange(scenario_geography)

#Scenario_AnalysisInput_2022_NGFS_REMIND <- Scenario_AnalysisInput_2022_NGFS_REMIND %>% tribble_paste()
tibble::tribble(
    ~scenario_geography, ~ald_sector,
                 "Asia",      "Coal",
                 "Asia",   "Oil&Gas",
                 "Asia",     "Power",
                "China",      "Coal",
                "China",   "Oil&Gas",
                "China",     "Power",
               "Global",      "Coal",
               "Global",   "Oil&Gas",
               "Global",     "Power",
                "India",      "Coal",
                "India",   "Oil&Gas",
                "India",     "Power",
                "Japan",      "Coal",
                "Japan",   "Oil&Gas",
                "Japan",     "Power",
         "LatinAmerica",      "Coal",
         "LatinAmerica",   "Oil&Gas",
         "LatinAmerica",     "Power",
  "MiddleEastAndAfrica",      "Coal",
  "MiddleEastAndAfrica",   "Oil&Gas",
  "MiddleEastAndAfrica",     "Power",
            "OecdAndEu",      "Coal",
            "OecdAndEu",   "Oil&Gas",
            "OecdAndEu",     "Power",
   "ReformingEconomies",      "Coal",
   "ReformingEconomies",   "Oil&Gas",
   "ReformingEconomies",     "Power",
         "UnitedStates",      "Coal",
         "UnitedStates",   "Oil&Gas",
         "UnitedStates",     "Power"
  )

# NGFS MESSAGE 
#7 regions 
Scenario_AnalysisInput_2022_MESSAGE_B2DS <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_B2DS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_MESSAGE_CP <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_CP")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_MESSAGE_DT <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_DT")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_MESSAGE_DN0 <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_DN0")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_MESSAGE_NDC <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_NDC")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_MESSAGE_NZ2050 <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_NZ2050")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

# #combine MESSAGE Scenarios 
# 7full scenario geographies 
Scenario_AnalysisInput_2022_NGFS_MESSAGE <- Scenario_AnalysisInput_2022_MESSAGE_B2DS %>%
  select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2022_MESSAGE_CP %>%
    select(scenario_geography, ald_sector) %>%
    inner_join(Scenario_AnalysisInput_2022_MESSAGE_DN0 %>%
      select(scenario_geography, ald_sector) %>%
      inner_join(Scenario_AnalysisInput_2022_MESSAGE_DT %>%
        select(scenario_geography, ald_sector) %>%
  select(scenario_geography, ald_sector))))  %>% 
  arrange(scenario_geography)

#Scenario_AnalysisInput_2022_NGFS_MESSAGE <- Scenario_AnalysisInput_2022_NGFS_MESSAGE %>% tribble_paste()
tibble::tribble(
    ~scenario_geography, ~ald_sector,
                 "Asia",      "Coal",
                 "Asia",   "Oil&Gas",
                 "Asia",     "Power",
                "China",      "Coal",
                "China",   "Oil&Gas",
                "China",     "Power",
               "Global",      "Coal",
               "Global",   "Oil&Gas",
               "Global",     "Power",
         "LatinAmerica",      "Coal",
         "LatinAmerica",   "Oil&Gas",
         "LatinAmerica",     "Power",
  "MiddleEastAndAfrica",      "Coal",
  "MiddleEastAndAfrica",   "Oil&Gas",
  "MiddleEastAndAfrica",     "Power",
            "OecdAndEu",      "Coal",
            "OecdAndEu",   "Oil&Gas",
            "OecdAndEu",     "Power",
   "ReformingEconomies",      "Coal",
   "ReformingEconomies",   "Oil&Gas",
   "ReformingEconomies",     "Power"
  )

###############################################################################
# Oxford scenario

Scenario_AnalysisInput_2022_Oxford_base <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("Oxford2021_base")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_Oxford_fast <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("Oxford2021_fast")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

# Binding Oxford scenarios
Scenario_AnalysisInput_2022_oxford <- Scenario_AnalysisInput_2022_Oxford_base %>%
  select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2022_Oxford_fast %>%
    select(scenario_geography, ald_sector))

# Scenario_AnalysisInput_2022_oxford <- Scenario_AnalysisInput_2022_oxford %>% tribble_paste()
tibble::tribble(
  ~scenario_geography, ~ald_sector,
             "Global",     "Power",
             "Global",      "Coal",
             "Global",   "Oil&Gas"
  )

###############################################################################
# IPR scenario
# 21 regions 
Scenario_AnalysisInput_2022_IPR_FPS <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("IPR2021_FPS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2022_IPR_RPS <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("IPR2021_RPS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

#19 regions 
Scenario_AnalysisInput_2022_IPR_baseline <- Scenario_AnalysisInput_2022 %>%
  filter(scenario %in% c("IPR2021_baseline")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

# Binding IPR scenarios needs to be inner_join
#5 regions 
Scenario_AnalysisInput_2022_ipr <- Scenario_AnalysisInput_2022_IPR_FPS %>%
  select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2022_IPR_RPS %>%
    select(scenario_geography, ald_sector) %>%
    inner_join(Scenario_AnalysisInput_2022_IPR_baseline)) %>%
  select(scenario_geography, ald_sector)

# Scenario_AnalysisInput_2022_ipr <- Scenario_AnalysisInput_2022_ipr %>% tribble_paste()
# NOTE TO BERTRAND: RUSSIA 
tibble::tribble(
  ~scenario_geography, ~ald_sector,
             "Brazil",     "Power",
             "Global",      "Coal",
             "Global",   "Oil&Gas",
             "Global",     "Power",
              "India",     "Power",
              "Japan",     "Power",
             "Russia",     "Power",
       "UnitedStates",     "Power"
  )
###############################################################################
# prewrangled_capacity_factors
# NOTE: Only relevant for power sector

prewrangled_capacity_factors <- readr::read_csv(
  file.path("data-raw", glue::glue("prewrangled_capacity_factors.csv"))
)

# WEO capacity factors
# 21 regions 
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

#prewrangled_capacity_factors_WEO_2021_scenarios <- prewrangled_capacity_factors_WEO_2021_scenarios %>% tribble_paste()
tibble::tribble(
       ~scenario_geography,
       "AdvancedEconomies",
                  "Africa",
             "AsiaPacific",
                  "Brazil",
  "CentralAndSouthAmerica",
                   "China",
     "DevelopingEconomies",
                 "Eurasia",
                  "Europe",
           "EuropeanUnion",
                  "Global",
                   "India",
                   "Japan",
              "MiddleEast",
                "Non-OECD",
            "NorthAmerica",
                    "OECD",
                  "Russia",
             "SouthAfrica",
           "SoutheastAsia",
            "UnitedStates"
  )

###############################################################################
# NGFS capacity factors

#GCAM
#10 regions 
prewrangled_capacity_factors_GCAM_B2DS <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_GCAM_B2DS")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_GCAM_CP <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_GCAM_CP")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_GCAM_DN0 <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_GCAM_DN0")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_GCAM_DT <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_GCAM_DT")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_GCAM_NDC <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_GCAM_NDC")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_GCAM_NZ2050 <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_GCAM_NZ2050")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_GCAM <- prewrangled_capacity_factors_GCAM_B2DS %>%
               inner_join(prewrangled_capacity_factors_GCAM_CP %>%
                            select(scenario_geography)) %>%
               inner_join(prewrangled_capacity_factors_GCAM_DN0 %>%
                            select(scenario_geography)) %>%
               inner_join(prewrangled_capacity_factors_GCAM_DT %>%
                            select(scenario_geography)) %>%
               inner_join(prewrangled_capacity_factors_GCAM_NDC %>%
                            select(scenario_geography)) %>%
               inner_join(prewrangled_capacity_factors_GCAM_NZ2050 %>%
                            select(scenario_geography)) %>%
               select(scenario_geography) %>%
  arrange(scenario_geography)

#prewrangled_capacity_factors_GCAM <- prewrangled_capacity_factors_GCAM %>% tribble_paste()
tibble::tribble(
    ~scenario_geography,
                 "Asia",
                "China",
               "Global",
            "Indonesia",
                "Japan",
         "LatinAmerica",
  "MiddleEastAndAfrica",
            "OecdAndEu",
   "ReformingEconomies",
         "UnitedStates"
  )

#REMIND 
#10 regions 
prewrangled_capacity_factors_REMIND_B2DS <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_REMIND_B2DS")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_REMIND_CP <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_REMIND_CP")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_REMIND_DN0 <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_REMIND_DN0")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_REMIND_DT <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_REMIND_DT")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_REMIND_NDC <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_REMIND_NDC")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_REMIND_NZ2050 <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_REMIND_NZ2050")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_REMIND <- prewrangled_capacity_factors_REMIND_B2DS %>%
  inner_join(prewrangled_capacity_factors_REMIND_CP %>%
               select(scenario_geography)) %>%
  inner_join(prewrangled_capacity_factors_REMIND_DN0 %>%
               select(scenario_geography)) %>%
  inner_join(prewrangled_capacity_factors_REMIND_DT %>%
               select(scenario_geography)) %>%
  inner_join(prewrangled_capacity_factors_REMIND_NDC %>%
               select(scenario_geography)) %>%
  inner_join(prewrangled_capacity_factors_REMIND_NZ2050 %>%
               select(scenario_geography)) %>%
  select(scenario_geography) %>%
  arrange(scenario_geography)

#prewrangled_capacity_factors_REMIND <- prewrangled_capacity_factors_REMIND %>% tribble_paste()
tibble::tribble(
    ~scenario_geography,
                 "Asia",
                "China",
               "Global",
                "India",
                "Japan",
         "LatinAmerica",
  "MiddleEastAndAfrica",
            "OecdAndEu",
   "ReformingEconomies",
         "UnitedStates"
  )

#MESSAGE
#7 regions 
prewrangled_capacity_factors_MESSAGE_B2DS <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_B2DS")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_MESSAGE_CP <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_CP")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_MESSAGE_DN0 <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_DN0")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_MESSAGE_DT <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_DT")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_MESSAGE_NDC <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_NDC")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_MESSAGE_NZ2050 <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_NZ2050")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_MESSAGE <- prewrangled_capacity_factors_MESSAGE_B2DS %>%
  inner_join(prewrangled_capacity_factors_MESSAGE_CP %>%
               select(scenario_geography)) %>%
  inner_join(prewrangled_capacity_factors_MESSAGE_DN0 %>%
               select(scenario_geography)) %>%
  inner_join(prewrangled_capacity_factors_MESSAGE_DT %>%
               select(scenario_geography)) %>%
  inner_join(prewrangled_capacity_factors_MESSAGE_NDC %>%
               select(scenario_geography)) %>%
  inner_join(prewrangled_capacity_factors_MESSAGE_NZ2050 %>%
               select(scenario_geography)) %>%
  select(scenario_geography) %>%
  arrange(scenario_geography)

#prewrangled_capacity_factors_MESSAGE <- prewrangled_capacity_factors_MESSAGE %>% tribble_paste()
tibble::tribble(
    ~scenario_geography,
                 "Asia",
                "China",
               "Global",
         "LatinAmerica",
  "MiddleEastAndAfrica",
            "OecdAndEu",
   "ReformingEconomies"
  )

###############################################################################
## Oxford capacity factors
#1 region
prewrangled_capacity_factors_Oxford_base <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("Oxford2021_base")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_Oxford_fast <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("Oxford2021_fast")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_oxford_scenarios <- prewrangled_capacity_factors_Oxford_base %>%
  select(scenario_geography) %>%
  inner_join(prewrangled_capacity_factors_Oxford_fast %>%
    select(scenario_geography))

# prewrangled_capacity_factors_oxford_scenarios %>% tribble_paste()
tibble::tribble(
  ~scenario_geography,
  "Global"
)

###############################################################################
## IPR capacity factors

# 21 regions 
prewrangled_capacity_factors_IPR_FPS <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("IPR2021_FPS")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_IPR_RPS <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("IPR2021_RPS")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_IPR_baseline <- prewrangled_capacity_factors %>%
  filter(scenario %in% c("IPR2021_baseline")) %>%
  select(scenario, scenario_geography) %>%
  distinct_all()

prewrangled_capacity_factors_ipr_scenarios <- prewrangled_capacity_factors_IPR_FPS %>%
  select(scenario_geography) %>%
  inner_join(prewrangled_capacity_factors_IPR_RPS %>%
    select(scenario_geography)) %>%
  inner_join(prewrangled_capacity_factors_IPR_baseline %>%
    select(scenario_geography))

#prewrangled_capacity_factors_ipr_scenarios %>% tribble_paste()
tibble::tribble(
  ~scenario_geography,
          "Australia",
             "Brazil",
             "Canada",
       "CoastalChina",
                "CSA",
                "EEU",
               "EURA",
                "GCC",
              "India",
              "Japan",
               "MENA",
                "RUS",
                 "SA",
        "SouthAfrica",
               "SEAO",
         "SouthKorea",
                "SSA",
                "GBR",
       "UnitedStates",
                "WEU",
             "Global"
  )

##############################################################################

## scenario geographies that are whitelisted need to be present in production data
abcd_stress_test_input <- r2dii.utils::path_dropbox_2dii("ST_INPUTS", "ST_INPUTS_MASTER", "abcd_stress_test_input.csv")

abcd_stress_test_input <- readr::read_csv(
  abcd_stress_test_input
)

abcd_stress_test_geographies <- abcd_stress_test_input %>%
  select(scenario_geography) %>%
  distinct_all()

# abcd_stress_test_geographies %>% tribble_paste()
tibble::tribble(
                     ~scenario_geography,
                       "EEurope_Eurasia",
                   "EEurope_EurasiaWoRU",
                               "Eurasia",
                                "Global",
                              "Non-OECD",
                              "OtherCIS",
                    "ReformingEconomies",
  "EmergingMarketAndDevelopingEconomies",
                                  "EURA",
                          "LatinAmerica",
                  "LatinAmericaExBrazil",
                    "RestCentralAmerica",
                          "NorthAmerica",
                                  "OECD",
                             "OecdAndEu",
                          "OECDAmericas",
                          "UnitedStates",
                     "AdvancedEconomies",
                           "AsiaPacific",
                             "Australia",
                       "OECDAsiaOceania",
                   "OECDAsiaOceaniaWoJP",
                                "Africa",
                            "AfricaWoZA",
                   "MiddleEastAndAfrica",
                  "RestSubSaharanAfrica",
                                   "SSA",
                      "RestSouthAmerica",
                                   "CSA",
                       "AlgeriaAndLibya",
                                  "MENA",
                                 "ASEAN",
                                  "Asia",
                              "Malaysia",
                           "NonOECDAsia",
                       "NonOECDAsiaRest",
                                  "SEAO",
                                 "Egypt",
                               "EFTA+UK",
                         "EuropeanUnion",
                                "Europe",
                            "OECDEurope",
                                   "GBR",
                            "MiddleEast",
                              "RestGulf",
                                   "GCC",
                             "Argentina",
                                "Canada",
                      "NorthAmericaExUS",
                      "OECDAmericasWoUS",
                             "Indonesia",
                                 "India",
                                  "EU27",
                                   "WEU",
                                  "EFTA",
                               "Vietnam",
                                 "China",
                          "CoastalChina",
                                   "EEU",
                              "Thailand",
                                "Russia",
                                   "RUS",
                                "Mexico",
                     "RestSouthEastAsia",
                                "Brazil",
                "MediteraneanMiddleEast",
                            "NewZealand",
                         "RestSouthAsia",
                                    "SA",
                     "MoroccoAndTunisia",
                           "RestPacific",
                                "Turkey",
                           "NonOECDRest",
                           "SouthAfrica",
                                 "Japan",
                               "Ukraine",
                                 "Chile",
                         "OthersBalkans",
                                  "Iran",
                            "SouthKorea",
                           "SaudiArabia"
  )

###############################################################################
# Supported overlap -------------------------------------------------------
# We can only offer scenario_geography x region x sector combinations for which we have
# all data

# removing scenario_geography x Power combinations that are missing in capacity factors 
overlap_all_weo <- Scenario_AnalysisInput_2022_WEO_without_NZE_2050 %>%
  filter(!(ald_sector == "Power" & !.data$scenario_geography %in% prewrangled_capacity_factors_WEO_2021_scenarios$scenario_geography)) 

## NOTE: excluded  is the power sector in Latin America and EU27 and EmergingMarketAndDevelopingEconomies, as not present in capacity factors
excluded_weo <- setdiff(Scenario_AnalysisInput_2022_WEO_without_NZE_2050, overlap_all_weo)

## geographies overlap with
overlap_all_weo <- overlap_all_weo %>% inner_join(abcd_stress_test_geographies) 

# full scenarios
overlap_all_weo$scenario_STEPS <- "WEO2021_STEPS"
overlap_all_weo$scenario_SDS <- "WEO2021_SDS"
overlap_all_weo$scenario_APS <- "WEO2021_APS"

overlap_all_weo <- overlap_all_weo %>%
  pivot_longer(scenario_STEPS:scenario_APS, values_to = "scenario") %>%
  select(-c(name))

overlap_all_weo <- overlap_all_weo %>% arrange(scenario_geography, scenario)

overlap_all_weo <- overlap_all_weo %>% rbind(Scenario_AnalysisInput_2022_NZE_2050) 

#18 geographies in total
#overlap_all_weo <- overlap_all_weo %>% tribble_paste()
################################################################################
# Overlap GECO

overlap_all_weo_geco <- overlap_all_weo %>% rbind(Scenario_AnalysisInput_2022_GECO)

###############################################################################
## Overlap NGFS

#GCAM Overlap 

overlap_gcam_ngfs <- Scenario_AnalysisInput_2022_NGFS_GCAM %>%
  dplyr::filter(!(ald_sector == "Power" & !.data$scenario_geography %in% prewrangled_capacity_factors_GCAM$scenario_geography))

excluded_gcam_ngfs <- setdiff(Scenario_AnalysisInput_2022_NGFS_GCAM, overlap_gcam_ngfs)

## geographies overlap with production data ##this is problematic here
overlap_gcam_ngfs <- overlap_gcam_ngfs %>% inner_join(abcd_stress_test_geographies)

overlap_gcam_ngfs$scenario_GCAM_B2DS <- "NGFS2021_GCAM_B2DS"
overlap_gcam_ngfs$scenario_GCAM_CP <- "NGFS2021_GCAM_CP"
overlap_gcam_ngfs$scenario_GCAM_DN0 <- "NGFS2021_GCAM_DN0"
overlap_gcam_ngfs$scenario_GCAM_DT <- "NGFS2021_GCAM_DT"
overlap_gcam_ngfs$scenario_GCAM_NDC <- "NGFS2021_GCAM_NDC"
overlap_gcam_ngfs$scenario_GCAM_NZ2050 <- "NGFS2021_GCAM_NZ2050"

overlap_gcam_ngfs <- overlap_gcam_ngfs %>%
  pivot_longer(scenario_GCAM_B2DS:scenario_GCAM_NZ2050, values_to = "scenario") %>%
  select(-c(name))

overlap_gcam_ngfs <- overlap_gcam_ngfs %>% arrange(scenario_geography, scenario)

#REMIND Overlap 

overlap_remind_ngfs <- Scenario_AnalysisInput_2022_NGFS_REMIND %>%
  filter(!(ald_sector == "Power" & !.data$scenario_geography %in% prewrangled_capacity_factors_REMIND$scenario_geography))

excluded_remind_ngfs <- setdiff(Scenario_AnalysisInput_2022_NGFS_REMIND, overlap_remind_ngfs)


## geographies overlap with production data ##this is problematic here
overlap_remind_ngfs <- overlap_remind_ngfs %>% inner_join(abcd_stress_test_geographies)

overlap_remind_ngfs$scenario_REMIND_B2DS <- "NGFS2021_REMIND_B2DS"
overlap_remind_ngfs$scenario_REMIND_CP <- "NGFS2021_REMIND_CP"
overlap_remind_ngfs$scenario_REMIND_DN0 <- "NGFS2021_REMIND_DN0"
overlap_remind_ngfs$scenario_REMIND_DT <- "NGFS2021_REMIND_DT"
overlap_remind_ngfs$scenario_REMIND_NDC <- "NGFS2021_REMIND_NDC"
overlap_remind_ngfs$scenario_REMIND_NZ2050 <- "NGFS2021_REMIND_NZ2050"

overlap_remind_ngfs <- overlap_remind_ngfs %>%
  pivot_longer(scenario_REMIND_B2DS:scenario_REMIND_NZ2050, values_to = "scenario") %>%
  select(-c(name))

overlap_remind_ngfs <- overlap_remind_ngfs %>% arrange(scenario_geography, scenario)

#MESSAGE Overlap

overlap_message_ngfs <- Scenario_AnalysisInput_2022_NGFS_MESSAGE %>%
  filter(!(ald_sector == "Power" & !.data$scenario_geography %in% prewrangled_capacity_factors_MESSAGE$scenario_geography))

excluded_message_ngfs <- setdiff(Scenario_AnalysisInput_2022_NGFS_MESSAGE, overlap_message_ngfs)


## geographies overlap with production data ##this is problematic here
overlap_message_ngfs <- overlap_message_ngfs %>% inner_join(abcd_stress_test_geographies)

overlap_message_ngfs$scenario_MESSAGE_B2DS <- "NGFS2021_MESSAGE_B2DS"
overlap_message_ngfs$scenario_MESSAGE_CP <- "NGFS2021_MESSAGE_CP"
overlap_message_ngfs$scenario_MESSAGE_DN0 <- "NGFS2021_MESSAGE_DN0"
overlap_message_ngfs$scenario_MESSAGE_DT <- "NGFS2021_MESSAGE_DT"
overlap_message_ngfs$scenario_MESSAGE_NDC <- "NGFS2021_MESSAGE_NDC"
overlap_message_ngfs$scenario_MESSAGE_NZ2050 <- "NGFS2021_MESSAGE_NZ2050"

overlap_message_ngfs <- overlap_message_ngfs %>%
  pivot_longer(scenario_MESSAGE_B2DS:scenario_MESSAGE_NZ2050, values_to = "scenario") %>%
  select(-c(name))

overlap_remind_ngfs <- overlap_remind_ngfs %>% arrange(scenario_geography, scenario)

overlap_all_ngfs <- overlap_remind_ngfs %>%
  rbind(overlap_gcam_ngfs) %>%
  rbind(overlap_message_ngfs) %>% 
  arrange(scenario_geography, scenario)

################################################################################
## Overlap Oxford
## overlap with IPR capacity factors
overlap_all_oxford <- Scenario_AnalysisInput_2022_oxford %>%
  filter(!(ald_sector == "Power" & !.data$scenario_geography %in% prewrangled_capacity_factors_oxford_scenarios$scenario_geography))

excluded_oxford <- setdiff(Scenario_AnalysisInput_2022_oxford, overlap_all_oxford)


## geographies overlap with production data
overlap_all_oxford <- overlap_all_oxford %>% inner_join(abcd_stress_test_geographies)

overlap_all_oxford$scenario_fast <- "Oxford2021_fast"
overlap_all_oxford$scenario_base <- "Oxford2021_base"


overlap_all_oxford <- overlap_all_oxford %>%
  pivot_longer(scenario_fast:scenario_base, values_to = "scenario") %>%
  select(-c(name))

overlap_all_oxford <- overlap_all_oxford %>% arrange(scenario_geography, scenario)

##############################################################################
#### Overlap IPR
## overlap with IPR capacity factors
overlap_all_ipr <- Scenario_AnalysisInput_2022_ipr %>%
  filter(!(ald_sector == "Power" & !.data$scenario_geography %in% prewrangled_capacity_factors_ipr_scenarios$scenario_geography))

excluded_ipr <- setdiff(Scenario_AnalysisInput_2022_ipr, overlap_all_ipr)

## geographies overlap with production data
overlap_all_ipr <- overlap_all_ipr %>% inner_join(abcd_stress_test_geographies)

overlap_all_ipr$scenario_FPS <- "IPR2021_FPS"
overlap_all_ipr$scenario_RPS <- "IPR2021_RPS"
overlap_all_ipr$scenario_baseline <- "IPR2021_baseline"

overlap_all_ipr <- overlap_all_ipr %>%
  pivot_longer(scenario_FPS:scenario_baseline, values_to = "scenario") %>%
  select(-c(name))

overlap_all_ipr <- overlap_all_ipr %>% arrange(scenario_geography, scenario)
###############################################################################
## joining all scenarios
overlap_all_combined <- full_join(overlap_all_weo_geco, overlap_all_ngfs) %>% arrange(scenario_geography, scenario)
overlap_all_combined <- full_join(overlap_all_combined, overlap_all_ipr) %>% arrange(scenario_geography, scenario)
overlap_all_combined <- full_join(overlap_all_combined, overlap_all_oxford) %>% arrange(scenario_geography, scenario)

#overlap_all_combined <- overlap_all_combined %>% tribble_paste()
dp_set_max_rows(7000) 


tibble::tribble(
                     ~scenario_geography,  ~ald_sector,                 ~scenario,
                     "AdvancedEconomies",       "Coal",             "WEO2021_APS",
                     "AdvancedEconomies",    "Oil&Gas",             "WEO2021_APS",
                     "AdvancedEconomies",      "Power",             "WEO2021_APS",
                     "AdvancedEconomies",       "Coal",             "WEO2021_SDS",
                     "AdvancedEconomies",    "Oil&Gas",             "WEO2021_SDS",
                     "AdvancedEconomies",      "Power",             "WEO2021_SDS",
                     "AdvancedEconomies",       "Coal",           "WEO2021_STEPS",
                     "AdvancedEconomies",    "Oil&Gas",           "WEO2021_STEPS",
                     "AdvancedEconomies",      "Power",           "WEO2021_STEPS",
                                "Africa",       "Coal",             "WEO2021_APS",
                                "Africa",    "Oil&Gas",             "WEO2021_APS",
                                "Africa",      "Power",             "WEO2021_APS",
                                "Africa",       "Coal",             "WEO2021_SDS",
                                "Africa",    "Oil&Gas",             "WEO2021_SDS",
                                "Africa",      "Power",             "WEO2021_SDS",
                                "Africa",       "Coal",           "WEO2021_STEPS",
                                "Africa",    "Oil&Gas",           "WEO2021_STEPS",
                                "Africa",      "Power",           "WEO2021_STEPS",
                                  "Asia",       "Coal",      "NGFS2021_GCAM_B2DS",
                                  "Asia",    "Oil&Gas",      "NGFS2021_GCAM_B2DS",
                                  "Asia",      "Power",      "NGFS2021_GCAM_B2DS",
                                  "Asia",       "Coal",        "NGFS2021_GCAM_CP",
                                  "Asia",    "Oil&Gas",        "NGFS2021_GCAM_CP",
                                  "Asia",      "Power",        "NGFS2021_GCAM_CP",
                                  "Asia",       "Coal",       "NGFS2021_GCAM_DN0",
                                  "Asia",    "Oil&Gas",       "NGFS2021_GCAM_DN0",
                                  "Asia",      "Power",       "NGFS2021_GCAM_DN0",
                                  "Asia",       "Coal",        "NGFS2021_GCAM_DT",
                                  "Asia",    "Oil&Gas",        "NGFS2021_GCAM_DT",
                                  "Asia",      "Power",        "NGFS2021_GCAM_DT",
                                  "Asia",       "Coal",       "NGFS2021_GCAM_NDC",
                                  "Asia",    "Oil&Gas",       "NGFS2021_GCAM_NDC",
                                  "Asia",      "Power",       "NGFS2021_GCAM_NDC",
                                  "Asia",       "Coal",    "NGFS2021_GCAM_NZ2050",
                                  "Asia",    "Oil&Gas",    "NGFS2021_GCAM_NZ2050",
                                  "Asia",      "Power",    "NGFS2021_GCAM_NZ2050",
                                  "Asia",       "Coal",   "NGFS2021_MESSAGE_B2DS",
                                  "Asia",    "Oil&Gas",   "NGFS2021_MESSAGE_B2DS",
                                  "Asia",      "Power",   "NGFS2021_MESSAGE_B2DS",
                                  "Asia",       "Coal",     "NGFS2021_MESSAGE_CP",
                                  "Asia",    "Oil&Gas",     "NGFS2021_MESSAGE_CP",
                                  "Asia",      "Power",     "NGFS2021_MESSAGE_CP",
                                  "Asia",       "Coal",    "NGFS2021_MESSAGE_DN0",
                                  "Asia",    "Oil&Gas",    "NGFS2021_MESSAGE_DN0",
                                  "Asia",      "Power",    "NGFS2021_MESSAGE_DN0",
                                  "Asia",       "Coal",     "NGFS2021_MESSAGE_DT",
                                  "Asia",    "Oil&Gas",     "NGFS2021_MESSAGE_DT",
                                  "Asia",      "Power",     "NGFS2021_MESSAGE_DT",
                                  "Asia",       "Coal",    "NGFS2021_MESSAGE_NDC",
                                  "Asia",    "Oil&Gas",    "NGFS2021_MESSAGE_NDC",
                                  "Asia",      "Power",    "NGFS2021_MESSAGE_NDC",
                                  "Asia",       "Coal", "NGFS2021_MESSAGE_NZ2050",
                                  "Asia",    "Oil&Gas", "NGFS2021_MESSAGE_NZ2050",
                                  "Asia",      "Power", "NGFS2021_MESSAGE_NZ2050",
                                  "Asia",       "Coal",    "NGFS2021_REMIND_B2DS",
                                  "Asia",    "Oil&Gas",    "NGFS2021_REMIND_B2DS",
                                  "Asia",      "Power",    "NGFS2021_REMIND_B2DS",
                                  "Asia",       "Coal",      "NGFS2021_REMIND_CP",
                                  "Asia",    "Oil&Gas",      "NGFS2021_REMIND_CP",
                                  "Asia",      "Power",      "NGFS2021_REMIND_CP",
                                  "Asia",       "Coal",     "NGFS2021_REMIND_DN0",
                                  "Asia",    "Oil&Gas",     "NGFS2021_REMIND_DN0",
                                  "Asia",      "Power",     "NGFS2021_REMIND_DN0",
                                  "Asia",       "Coal",      "NGFS2021_REMIND_DT",
                                  "Asia",    "Oil&Gas",      "NGFS2021_REMIND_DT",
                                  "Asia",      "Power",      "NGFS2021_REMIND_DT",
                                  "Asia",       "Coal",     "NGFS2021_REMIND_NDC",
                                  "Asia",    "Oil&Gas",     "NGFS2021_REMIND_NDC",
                                  "Asia",      "Power",     "NGFS2021_REMIND_NDC",
                                  "Asia",       "Coal",  "NGFS2021_REMIND_NZ2050",
                                  "Asia",    "Oil&Gas",  "NGFS2021_REMIND_NZ2050",
                                  "Asia",      "Power",  "NGFS2021_REMIND_NZ2050",
                           "AsiaPacific",       "Coal",             "WEO2021_APS",
                           "AsiaPacific",    "Oil&Gas",             "WEO2021_APS",
                           "AsiaPacific",      "Power",             "WEO2021_APS",
                           "AsiaPacific",       "Coal",             "WEO2021_SDS",
                           "AsiaPacific",    "Oil&Gas",             "WEO2021_SDS",
                           "AsiaPacific",      "Power",             "WEO2021_SDS",
                           "AsiaPacific",       "Coal",           "WEO2021_STEPS",
                           "AsiaPacific",    "Oil&Gas",           "WEO2021_STEPS",
                           "AsiaPacific",      "Power",           "WEO2021_STEPS",
                                "Brazil",      "Power",             "IPR2021_FPS",
                                "Brazil",      "Power",             "IPR2021_RPS",
                                "Brazil",      "Power",        "IPR2021_baseline",
                                "Brazil",      "Power",             "WEO2021_APS",
                                "Brazil",      "Power",             "WEO2021_SDS",
                                "Brazil",      "Power",           "WEO2021_STEPS",
                                 "China",       "Coal",      "NGFS2021_GCAM_B2DS",
                                 "China",    "Oil&Gas",      "NGFS2021_GCAM_B2DS",
                                 "China",      "Power",      "NGFS2021_GCAM_B2DS",
                                 "China",       "Coal",        "NGFS2021_GCAM_CP",
                                 "China",    "Oil&Gas",        "NGFS2021_GCAM_CP",
                                 "China",      "Power",        "NGFS2021_GCAM_CP",
                                 "China",       "Coal",       "NGFS2021_GCAM_DN0",
                                 "China",    "Oil&Gas",       "NGFS2021_GCAM_DN0",
                                 "China",      "Power",       "NGFS2021_GCAM_DN0",
                                 "China",       "Coal",        "NGFS2021_GCAM_DT",
                                 "China",    "Oil&Gas",        "NGFS2021_GCAM_DT",
                                 "China",      "Power",        "NGFS2021_GCAM_DT",
                                 "China",       "Coal",       "NGFS2021_GCAM_NDC",
                                 "China",    "Oil&Gas",       "NGFS2021_GCAM_NDC",
                                 "China",      "Power",       "NGFS2021_GCAM_NDC",
                                 "China",       "Coal",    "NGFS2021_GCAM_NZ2050",
                                 "China",    "Oil&Gas",    "NGFS2021_GCAM_NZ2050",
                                 "China",      "Power",    "NGFS2021_GCAM_NZ2050",
                                 "China",       "Coal",   "NGFS2021_MESSAGE_B2DS",
                                 "China",    "Oil&Gas",   "NGFS2021_MESSAGE_B2DS",
                                 "China",      "Power",   "NGFS2021_MESSAGE_B2DS",
                                 "China",       "Coal",     "NGFS2021_MESSAGE_CP",
                                 "China",    "Oil&Gas",     "NGFS2021_MESSAGE_CP",
                                 "China",      "Power",     "NGFS2021_MESSAGE_CP",
                                 "China",       "Coal",    "NGFS2021_MESSAGE_DN0",
                                 "China",    "Oil&Gas",    "NGFS2021_MESSAGE_DN0",
                                 "China",      "Power",    "NGFS2021_MESSAGE_DN0",
                                 "China",       "Coal",     "NGFS2021_MESSAGE_DT",
                                 "China",    "Oil&Gas",     "NGFS2021_MESSAGE_DT",
                                 "China",      "Power",     "NGFS2021_MESSAGE_DT",
                                 "China",       "Coal",    "NGFS2021_MESSAGE_NDC",
                                 "China",    "Oil&Gas",    "NGFS2021_MESSAGE_NDC",
                                 "China",      "Power",    "NGFS2021_MESSAGE_NDC",
                                 "China",       "Coal", "NGFS2021_MESSAGE_NZ2050",
                                 "China",    "Oil&Gas", "NGFS2021_MESSAGE_NZ2050",
                                 "China",      "Power", "NGFS2021_MESSAGE_NZ2050",
                                 "China",       "Coal",    "NGFS2021_REMIND_B2DS",
                                 "China",    "Oil&Gas",    "NGFS2021_REMIND_B2DS",
                                 "China",      "Power",    "NGFS2021_REMIND_B2DS",
                                 "China",       "Coal",      "NGFS2021_REMIND_CP",
                                 "China",    "Oil&Gas",      "NGFS2021_REMIND_CP",
                                 "China",      "Power",      "NGFS2021_REMIND_CP",
                                 "China",       "Coal",     "NGFS2021_REMIND_DN0",
                                 "China",    "Oil&Gas",     "NGFS2021_REMIND_DN0",
                                 "China",      "Power",     "NGFS2021_REMIND_DN0",
                                 "China",       "Coal",      "NGFS2021_REMIND_DT",
                                 "China",    "Oil&Gas",      "NGFS2021_REMIND_DT",
                                 "China",      "Power",      "NGFS2021_REMIND_DT",
                                 "China",       "Coal",     "NGFS2021_REMIND_NDC",
                                 "China",    "Oil&Gas",     "NGFS2021_REMIND_NDC",
                                 "China",      "Power",     "NGFS2021_REMIND_NDC",
                                 "China",       "Coal",  "NGFS2021_REMIND_NZ2050",
                                 "China",    "Oil&Gas",  "NGFS2021_REMIND_NZ2050",
                                 "China",      "Power",  "NGFS2021_REMIND_NZ2050",
                                 "China",      "Power",             "WEO2021_APS",
                                 "China",      "Power",             "WEO2021_SDS",
                                 "China",      "Power",           "WEO2021_STEPS",
  "EmergingMarketAndDevelopingEconomies",       "Coal",             "WEO2021_APS",
  "EmergingMarketAndDevelopingEconomies",    "Oil&Gas",             "WEO2021_APS",
  "EmergingMarketAndDevelopingEconomies",       "Coal",             "WEO2021_SDS",
  "EmergingMarketAndDevelopingEconomies",    "Oil&Gas",             "WEO2021_SDS",
  "EmergingMarketAndDevelopingEconomies",       "Coal",           "WEO2021_STEPS",
  "EmergingMarketAndDevelopingEconomies",    "Oil&Gas",           "WEO2021_STEPS",
                               "Eurasia",       "Coal",             "WEO2021_APS",
                               "Eurasia",    "Oil&Gas",             "WEO2021_APS",
                               "Eurasia",      "Power",             "WEO2021_APS",
                               "Eurasia",       "Coal",             "WEO2021_SDS",
                               "Eurasia",    "Oil&Gas",             "WEO2021_SDS",
                               "Eurasia",      "Power",             "WEO2021_SDS",
                               "Eurasia",       "Coal",           "WEO2021_STEPS",
                               "Eurasia",    "Oil&Gas",           "WEO2021_STEPS",
                               "Eurasia",      "Power",           "WEO2021_STEPS",
                                "Europe",       "Coal",             "WEO2021_APS",
                                "Europe",    "Oil&Gas",             "WEO2021_APS",
                                "Europe",      "Power",             "WEO2021_APS",
                                "Europe",       "Coal",             "WEO2021_SDS",
                                "Europe",    "Oil&Gas",             "WEO2021_SDS",
                                "Europe",      "Power",             "WEO2021_SDS",
                                "Europe",       "Coal",           "WEO2021_STEPS",
                                "Europe",    "Oil&Gas",           "WEO2021_STEPS",
                                "Europe",      "Power",           "WEO2021_STEPS",
                                "Global", "Automotive",      "GECO2021_1.5C-Unif",
                                "Global", "Automotive",         "GECO2021_CurPol",
                                "Global", "Automotive",        "GECO2021_NDC-LTS",
                                "Global",       "Coal",             "IPR2021_FPS",
                                "Global",    "Oil&Gas",             "IPR2021_FPS",
                                "Global",      "Power",             "IPR2021_FPS",
                                "Global",       "Coal",             "IPR2021_RPS",
                                "Global",    "Oil&Gas",             "IPR2021_RPS",
                                "Global",      "Power",             "IPR2021_RPS",
                                "Global",       "Coal",        "IPR2021_baseline",
                                "Global",    "Oil&Gas",        "IPR2021_baseline",
                                "Global",      "Power",        "IPR2021_baseline",
                                "Global",       "Coal",      "NGFS2021_GCAM_B2DS",
                                "Global",    "Oil&Gas",      "NGFS2021_GCAM_B2DS",
                                "Global",      "Power",      "NGFS2021_GCAM_B2DS",
                                "Global",       "Coal",        "NGFS2021_GCAM_CP",
                                "Global",    "Oil&Gas",        "NGFS2021_GCAM_CP",
                                "Global",      "Power",        "NGFS2021_GCAM_CP",
                                "Global",       "Coal",       "NGFS2021_GCAM_DN0",
                                "Global",    "Oil&Gas",       "NGFS2021_GCAM_DN0",
                                "Global",      "Power",       "NGFS2021_GCAM_DN0",
                                "Global",       "Coal",        "NGFS2021_GCAM_DT",
                                "Global",    "Oil&Gas",        "NGFS2021_GCAM_DT",
                                "Global",      "Power",        "NGFS2021_GCAM_DT",
                                "Global",       "Coal",       "NGFS2021_GCAM_NDC",
                                "Global",    "Oil&Gas",       "NGFS2021_GCAM_NDC",
                                "Global",      "Power",       "NGFS2021_GCAM_NDC",
                                "Global",       "Coal",    "NGFS2021_GCAM_NZ2050",
                                "Global",    "Oil&Gas",    "NGFS2021_GCAM_NZ2050",
                                "Global",      "Power",    "NGFS2021_GCAM_NZ2050",
                                "Global",       "Coal",   "NGFS2021_MESSAGE_B2DS",
                                "Global",    "Oil&Gas",   "NGFS2021_MESSAGE_B2DS",
                                "Global",      "Power",   "NGFS2021_MESSAGE_B2DS",
                                "Global",       "Coal",     "NGFS2021_MESSAGE_CP",
                                "Global",    "Oil&Gas",     "NGFS2021_MESSAGE_CP",
                                "Global",      "Power",     "NGFS2021_MESSAGE_CP",
                                "Global",       "Coal",    "NGFS2021_MESSAGE_DN0",
                                "Global",    "Oil&Gas",    "NGFS2021_MESSAGE_DN0",
                                "Global",      "Power",    "NGFS2021_MESSAGE_DN0",
                                "Global",       "Coal",     "NGFS2021_MESSAGE_DT",
                                "Global",    "Oil&Gas",     "NGFS2021_MESSAGE_DT",
                                "Global",      "Power",     "NGFS2021_MESSAGE_DT",
                                "Global",       "Coal",    "NGFS2021_MESSAGE_NDC",
                                "Global",    "Oil&Gas",    "NGFS2021_MESSAGE_NDC",
                                "Global",      "Power",    "NGFS2021_MESSAGE_NDC",
                                "Global",       "Coal", "NGFS2021_MESSAGE_NZ2050",
                                "Global",    "Oil&Gas", "NGFS2021_MESSAGE_NZ2050",
                                "Global",      "Power", "NGFS2021_MESSAGE_NZ2050",
                                "Global",       "Coal",    "NGFS2021_REMIND_B2DS",
                                "Global",    "Oil&Gas",    "NGFS2021_REMIND_B2DS",
                                "Global",      "Power",    "NGFS2021_REMIND_B2DS",
                                "Global",       "Coal",      "NGFS2021_REMIND_CP",
                                "Global",    "Oil&Gas",      "NGFS2021_REMIND_CP",
                                "Global",      "Power",      "NGFS2021_REMIND_CP",
                                "Global",       "Coal",     "NGFS2021_REMIND_DN0",
                                "Global",    "Oil&Gas",     "NGFS2021_REMIND_DN0",
                                "Global",      "Power",     "NGFS2021_REMIND_DN0",
                                "Global",       "Coal",      "NGFS2021_REMIND_DT",
                                "Global",    "Oil&Gas",      "NGFS2021_REMIND_DT",
                                "Global",      "Power",      "NGFS2021_REMIND_DT",
                                "Global",       "Coal",     "NGFS2021_REMIND_NDC",
                                "Global",    "Oil&Gas",     "NGFS2021_REMIND_NDC",
                                "Global",      "Power",     "NGFS2021_REMIND_NDC",
                                "Global",       "Coal",  "NGFS2021_REMIND_NZ2050",
                                "Global",    "Oil&Gas",  "NGFS2021_REMIND_NZ2050",
                                "Global",      "Power",  "NGFS2021_REMIND_NZ2050",
                                "Global",      "Power",         "Oxford2021_base",
                                "Global",       "Coal",         "Oxford2021_base",
                                "Global",    "Oil&Gas",         "Oxford2021_base",
                                "Global",      "Power",         "Oxford2021_fast",
                                "Global",       "Coal",         "Oxford2021_fast",
                                "Global",    "Oil&Gas",         "Oxford2021_fast",
                                "Global",       "Coal",             "WEO2021_APS",
                                "Global",    "Oil&Gas",             "WEO2021_APS",
                                "Global",      "Power",             "WEO2021_APS",
                                "Global", "Automotive",        "WEO2021_NZE_2050",
                                "Global",       "Coal",        "WEO2021_NZE_2050",
                                "Global",    "Oil&Gas",        "WEO2021_NZE_2050",
                                "Global",      "Power",        "WEO2021_NZE_2050",
                                "Global",       "Coal",             "WEO2021_SDS",
                                "Global",    "Oil&Gas",             "WEO2021_SDS",
                                "Global",      "Power",             "WEO2021_SDS",
                                "Global",       "Coal",           "WEO2021_STEPS",
                                "Global",    "Oil&Gas",           "WEO2021_STEPS",
                                "Global",      "Power",           "WEO2021_STEPS",
                                 "India",      "Power",             "IPR2021_FPS",
                                 "India",      "Power",             "IPR2021_RPS",
                                 "India",      "Power",        "IPR2021_baseline",
                                 "India",       "Coal",    "NGFS2021_REMIND_B2DS",
                                 "India",    "Oil&Gas",    "NGFS2021_REMIND_B2DS",
                                 "India",      "Power",    "NGFS2021_REMIND_B2DS",
                                 "India",       "Coal",      "NGFS2021_REMIND_CP",
                                 "India",    "Oil&Gas",      "NGFS2021_REMIND_CP",
                                 "India",      "Power",      "NGFS2021_REMIND_CP",
                                 "India",       "Coal",     "NGFS2021_REMIND_DN0",
                                 "India",    "Oil&Gas",     "NGFS2021_REMIND_DN0",
                                 "India",      "Power",     "NGFS2021_REMIND_DN0",
                                 "India",       "Coal",      "NGFS2021_REMIND_DT",
                                 "India",    "Oil&Gas",      "NGFS2021_REMIND_DT",
                                 "India",      "Power",      "NGFS2021_REMIND_DT",
                                 "India",       "Coal",     "NGFS2021_REMIND_NDC",
                                 "India",    "Oil&Gas",     "NGFS2021_REMIND_NDC",
                                 "India",      "Power",     "NGFS2021_REMIND_NDC",
                                 "India",       "Coal",  "NGFS2021_REMIND_NZ2050",
                                 "India",    "Oil&Gas",  "NGFS2021_REMIND_NZ2050",
                                 "India",      "Power",  "NGFS2021_REMIND_NZ2050",
                                 "India",      "Power",             "WEO2021_APS",
                                 "India",      "Power",             "WEO2021_SDS",
                                 "India",      "Power",           "WEO2021_STEPS",
                             "Indonesia",       "Coal",      "NGFS2021_GCAM_B2DS",
                             "Indonesia",    "Oil&Gas",      "NGFS2021_GCAM_B2DS",
                             "Indonesia",      "Power",      "NGFS2021_GCAM_B2DS",
                             "Indonesia",       "Coal",        "NGFS2021_GCAM_CP",
                             "Indonesia",    "Oil&Gas",        "NGFS2021_GCAM_CP",
                             "Indonesia",      "Power",        "NGFS2021_GCAM_CP",
                             "Indonesia",       "Coal",       "NGFS2021_GCAM_DN0",
                             "Indonesia",    "Oil&Gas",       "NGFS2021_GCAM_DN0",
                             "Indonesia",      "Power",       "NGFS2021_GCAM_DN0",
                             "Indonesia",       "Coal",        "NGFS2021_GCAM_DT",
                             "Indonesia",    "Oil&Gas",        "NGFS2021_GCAM_DT",
                             "Indonesia",      "Power",        "NGFS2021_GCAM_DT",
                             "Indonesia",       "Coal",       "NGFS2021_GCAM_NDC",
                             "Indonesia",    "Oil&Gas",       "NGFS2021_GCAM_NDC",
                             "Indonesia",      "Power",       "NGFS2021_GCAM_NDC",
                             "Indonesia",       "Coal",    "NGFS2021_GCAM_NZ2050",
                             "Indonesia",    "Oil&Gas",    "NGFS2021_GCAM_NZ2050",
                             "Indonesia",      "Power",    "NGFS2021_GCAM_NZ2050",
                                 "Japan",      "Power",             "IPR2021_FPS",
                                 "Japan",      "Power",             "IPR2021_RPS",
                                 "Japan",      "Power",        "IPR2021_baseline",
                                 "Japan",       "Coal",      "NGFS2021_GCAM_B2DS",
                                 "Japan",    "Oil&Gas",      "NGFS2021_GCAM_B2DS",
                                 "Japan",      "Power",      "NGFS2021_GCAM_B2DS",
                                 "Japan",       "Coal",        "NGFS2021_GCAM_CP",
                                 "Japan",    "Oil&Gas",        "NGFS2021_GCAM_CP",
                                 "Japan",      "Power",        "NGFS2021_GCAM_CP",
                                 "Japan",       "Coal",       "NGFS2021_GCAM_DN0",
                                 "Japan",    "Oil&Gas",       "NGFS2021_GCAM_DN0",
                                 "Japan",      "Power",       "NGFS2021_GCAM_DN0",
                                 "Japan",       "Coal",        "NGFS2021_GCAM_DT",
                                 "Japan",    "Oil&Gas",        "NGFS2021_GCAM_DT",
                                 "Japan",      "Power",        "NGFS2021_GCAM_DT",
                                 "Japan",       "Coal",       "NGFS2021_GCAM_NDC",
                                 "Japan",    "Oil&Gas",       "NGFS2021_GCAM_NDC",
                                 "Japan",      "Power",       "NGFS2021_GCAM_NDC",
                                 "Japan",       "Coal",    "NGFS2021_GCAM_NZ2050",
                                 "Japan",    "Oil&Gas",    "NGFS2021_GCAM_NZ2050",
                                 "Japan",      "Power",    "NGFS2021_GCAM_NZ2050",
                                 "Japan",       "Coal",    "NGFS2021_REMIND_B2DS",
                                 "Japan",    "Oil&Gas",    "NGFS2021_REMIND_B2DS",
                                 "Japan",      "Power",    "NGFS2021_REMIND_B2DS",
                                 "Japan",       "Coal",      "NGFS2021_REMIND_CP",
                                 "Japan",    "Oil&Gas",      "NGFS2021_REMIND_CP",
                                 "Japan",      "Power",      "NGFS2021_REMIND_CP",
                                 "Japan",       "Coal",     "NGFS2021_REMIND_DN0",
                                 "Japan",    "Oil&Gas",     "NGFS2021_REMIND_DN0",
                                 "Japan",      "Power",     "NGFS2021_REMIND_DN0",
                                 "Japan",       "Coal",      "NGFS2021_REMIND_DT",
                                 "Japan",    "Oil&Gas",      "NGFS2021_REMIND_DT",
                                 "Japan",      "Power",      "NGFS2021_REMIND_DT",
                                 "Japan",       "Coal",     "NGFS2021_REMIND_NDC",
                                 "Japan",    "Oil&Gas",     "NGFS2021_REMIND_NDC",
                                 "Japan",      "Power",     "NGFS2021_REMIND_NDC",
                                 "Japan",       "Coal",  "NGFS2021_REMIND_NZ2050",
                                 "Japan",    "Oil&Gas",  "NGFS2021_REMIND_NZ2050",
                                 "Japan",      "Power",  "NGFS2021_REMIND_NZ2050",
                                 "Japan",      "Power",             "WEO2021_APS",
                                 "Japan",      "Power",             "WEO2021_SDS",
                                 "Japan",      "Power",           "WEO2021_STEPS",
                          "LatinAmerica",       "Coal",      "NGFS2021_GCAM_B2DS",
                          "LatinAmerica",    "Oil&Gas",      "NGFS2021_GCAM_B2DS",
                          "LatinAmerica",      "Power",      "NGFS2021_GCAM_B2DS",
                          "LatinAmerica",       "Coal",        "NGFS2021_GCAM_CP",
                          "LatinAmerica",    "Oil&Gas",        "NGFS2021_GCAM_CP",
                          "LatinAmerica",      "Power",        "NGFS2021_GCAM_CP",
                          "LatinAmerica",       "Coal",       "NGFS2021_GCAM_DN0",
                          "LatinAmerica",    "Oil&Gas",       "NGFS2021_GCAM_DN0",
                          "LatinAmerica",      "Power",       "NGFS2021_GCAM_DN0",
                          "LatinAmerica",       "Coal",        "NGFS2021_GCAM_DT",
                          "LatinAmerica",    "Oil&Gas",        "NGFS2021_GCAM_DT",
                          "LatinAmerica",      "Power",        "NGFS2021_GCAM_DT",
                          "LatinAmerica",       "Coal",       "NGFS2021_GCAM_NDC",
                          "LatinAmerica",    "Oil&Gas",       "NGFS2021_GCAM_NDC",
                          "LatinAmerica",      "Power",       "NGFS2021_GCAM_NDC",
                          "LatinAmerica",       "Coal",    "NGFS2021_GCAM_NZ2050",
                          "LatinAmerica",    "Oil&Gas",    "NGFS2021_GCAM_NZ2050",
                          "LatinAmerica",      "Power",    "NGFS2021_GCAM_NZ2050",
                          "LatinAmerica",       "Coal",   "NGFS2021_MESSAGE_B2DS",
                          "LatinAmerica",    "Oil&Gas",   "NGFS2021_MESSAGE_B2DS",
                          "LatinAmerica",      "Power",   "NGFS2021_MESSAGE_B2DS",
                          "LatinAmerica",       "Coal",     "NGFS2021_MESSAGE_CP",
                          "LatinAmerica",    "Oil&Gas",     "NGFS2021_MESSAGE_CP",
                          "LatinAmerica",      "Power",     "NGFS2021_MESSAGE_CP",
                          "LatinAmerica",       "Coal",    "NGFS2021_MESSAGE_DN0",
                          "LatinAmerica",    "Oil&Gas",    "NGFS2021_MESSAGE_DN0",
                          "LatinAmerica",      "Power",    "NGFS2021_MESSAGE_DN0",
                          "LatinAmerica",       "Coal",     "NGFS2021_MESSAGE_DT",
                          "LatinAmerica",    "Oil&Gas",     "NGFS2021_MESSAGE_DT",
                          "LatinAmerica",      "Power",     "NGFS2021_MESSAGE_DT",
                          "LatinAmerica",       "Coal",    "NGFS2021_MESSAGE_NDC",
                          "LatinAmerica",    "Oil&Gas",    "NGFS2021_MESSAGE_NDC",
                          "LatinAmerica",      "Power",    "NGFS2021_MESSAGE_NDC",
                          "LatinAmerica",       "Coal", "NGFS2021_MESSAGE_NZ2050",
                          "LatinAmerica",    "Oil&Gas", "NGFS2021_MESSAGE_NZ2050",
                          "LatinAmerica",      "Power", "NGFS2021_MESSAGE_NZ2050",
                          "LatinAmerica",       "Coal",    "NGFS2021_REMIND_B2DS",
                          "LatinAmerica",    "Oil&Gas",    "NGFS2021_REMIND_B2DS",
                          "LatinAmerica",      "Power",    "NGFS2021_REMIND_B2DS",
                          "LatinAmerica",       "Coal",      "NGFS2021_REMIND_CP",
                          "LatinAmerica",    "Oil&Gas",      "NGFS2021_REMIND_CP",
                          "LatinAmerica",      "Power",      "NGFS2021_REMIND_CP",
                          "LatinAmerica",       "Coal",     "NGFS2021_REMIND_DN0",
                          "LatinAmerica",    "Oil&Gas",     "NGFS2021_REMIND_DN0",
                          "LatinAmerica",      "Power",     "NGFS2021_REMIND_DN0",
                          "LatinAmerica",       "Coal",      "NGFS2021_REMIND_DT",
                          "LatinAmerica",    "Oil&Gas",      "NGFS2021_REMIND_DT",
                          "LatinAmerica",      "Power",      "NGFS2021_REMIND_DT",
                          "LatinAmerica",       "Coal",     "NGFS2021_REMIND_NDC",
                          "LatinAmerica",    "Oil&Gas",     "NGFS2021_REMIND_NDC",
                          "LatinAmerica",      "Power",     "NGFS2021_REMIND_NDC",
                          "LatinAmerica",       "Coal",  "NGFS2021_REMIND_NZ2050",
                          "LatinAmerica",    "Oil&Gas",  "NGFS2021_REMIND_NZ2050",
                          "LatinAmerica",      "Power",  "NGFS2021_REMIND_NZ2050",
                          "LatinAmerica",       "Coal",             "WEO2021_APS",
                          "LatinAmerica",    "Oil&Gas",             "WEO2021_APS",
                          "LatinAmerica",       "Coal",             "WEO2021_SDS",
                          "LatinAmerica",    "Oil&Gas",             "WEO2021_SDS",
                          "LatinAmerica",       "Coal",           "WEO2021_STEPS",
                          "LatinAmerica",    "Oil&Gas",           "WEO2021_STEPS",
                            "MiddleEast",       "Coal",             "WEO2021_APS",
                            "MiddleEast",    "Oil&Gas",             "WEO2021_APS",
                            "MiddleEast",      "Power",             "WEO2021_APS",
                            "MiddleEast",       "Coal",             "WEO2021_SDS",
                            "MiddleEast",    "Oil&Gas",             "WEO2021_SDS",
                            "MiddleEast",      "Power",             "WEO2021_SDS",
                            "MiddleEast",       "Coal",           "WEO2021_STEPS",
                            "MiddleEast",    "Oil&Gas",           "WEO2021_STEPS",
                            "MiddleEast",      "Power",           "WEO2021_STEPS",
                   "MiddleEastAndAfrica",       "Coal",      "NGFS2021_GCAM_B2DS",
                   "MiddleEastAndAfrica",    "Oil&Gas",      "NGFS2021_GCAM_B2DS",
                   "MiddleEastAndAfrica",      "Power",      "NGFS2021_GCAM_B2DS",
                   "MiddleEastAndAfrica",       "Coal",        "NGFS2021_GCAM_CP",
                   "MiddleEastAndAfrica",    "Oil&Gas",        "NGFS2021_GCAM_CP",
                   "MiddleEastAndAfrica",      "Power",        "NGFS2021_GCAM_CP",
                   "MiddleEastAndAfrica",       "Coal",       "NGFS2021_GCAM_DN0",
                   "MiddleEastAndAfrica",    "Oil&Gas",       "NGFS2021_GCAM_DN0",
                   "MiddleEastAndAfrica",      "Power",       "NGFS2021_GCAM_DN0",
                   "MiddleEastAndAfrica",       "Coal",        "NGFS2021_GCAM_DT",
                   "MiddleEastAndAfrica",    "Oil&Gas",        "NGFS2021_GCAM_DT",
                   "MiddleEastAndAfrica",      "Power",        "NGFS2021_GCAM_DT",
                   "MiddleEastAndAfrica",       "Coal",       "NGFS2021_GCAM_NDC",
                   "MiddleEastAndAfrica",    "Oil&Gas",       "NGFS2021_GCAM_NDC",
                   "MiddleEastAndAfrica",      "Power",       "NGFS2021_GCAM_NDC",
                   "MiddleEastAndAfrica",       "Coal",    "NGFS2021_GCAM_NZ2050",
                   "MiddleEastAndAfrica",    "Oil&Gas",    "NGFS2021_GCAM_NZ2050",
                   "MiddleEastAndAfrica",      "Power",    "NGFS2021_GCAM_NZ2050",
                   "MiddleEastAndAfrica",       "Coal",   "NGFS2021_MESSAGE_B2DS",
                   "MiddleEastAndAfrica",    "Oil&Gas",   "NGFS2021_MESSAGE_B2DS",
                   "MiddleEastAndAfrica",      "Power",   "NGFS2021_MESSAGE_B2DS",
                   "MiddleEastAndAfrica",       "Coal",     "NGFS2021_MESSAGE_CP",
                   "MiddleEastAndAfrica",    "Oil&Gas",     "NGFS2021_MESSAGE_CP",
                   "MiddleEastAndAfrica",      "Power",     "NGFS2021_MESSAGE_CP",
                   "MiddleEastAndAfrica",       "Coal",    "NGFS2021_MESSAGE_DN0",
                   "MiddleEastAndAfrica",    "Oil&Gas",    "NGFS2021_MESSAGE_DN0",
                   "MiddleEastAndAfrica",      "Power",    "NGFS2021_MESSAGE_DN0",
                   "MiddleEastAndAfrica",       "Coal",     "NGFS2021_MESSAGE_DT",
                   "MiddleEastAndAfrica",    "Oil&Gas",     "NGFS2021_MESSAGE_DT",
                   "MiddleEastAndAfrica",      "Power",     "NGFS2021_MESSAGE_DT",
                   "MiddleEastAndAfrica",       "Coal",    "NGFS2021_MESSAGE_NDC",
                   "MiddleEastAndAfrica",    "Oil&Gas",    "NGFS2021_MESSAGE_NDC",
                   "MiddleEastAndAfrica",      "Power",    "NGFS2021_MESSAGE_NDC",
                   "MiddleEastAndAfrica",       "Coal", "NGFS2021_MESSAGE_NZ2050",
                   "MiddleEastAndAfrica",    "Oil&Gas", "NGFS2021_MESSAGE_NZ2050",
                   "MiddleEastAndAfrica",      "Power", "NGFS2021_MESSAGE_NZ2050",
                   "MiddleEastAndAfrica",       "Coal",    "NGFS2021_REMIND_B2DS",
                   "MiddleEastAndAfrica",    "Oil&Gas",    "NGFS2021_REMIND_B2DS",
                   "MiddleEastAndAfrica",      "Power",    "NGFS2021_REMIND_B2DS",
                   "MiddleEastAndAfrica",       "Coal",      "NGFS2021_REMIND_CP",
                   "MiddleEastAndAfrica",    "Oil&Gas",      "NGFS2021_REMIND_CP",
                   "MiddleEastAndAfrica",      "Power",      "NGFS2021_REMIND_CP",
                   "MiddleEastAndAfrica",       "Coal",     "NGFS2021_REMIND_DN0",
                   "MiddleEastAndAfrica",    "Oil&Gas",     "NGFS2021_REMIND_DN0",
                   "MiddleEastAndAfrica",      "Power",     "NGFS2021_REMIND_DN0",
                   "MiddleEastAndAfrica",       "Coal",      "NGFS2021_REMIND_DT",
                   "MiddleEastAndAfrica",    "Oil&Gas",      "NGFS2021_REMIND_DT",
                   "MiddleEastAndAfrica",      "Power",      "NGFS2021_REMIND_DT",
                   "MiddleEastAndAfrica",       "Coal",     "NGFS2021_REMIND_NDC",
                   "MiddleEastAndAfrica",    "Oil&Gas",     "NGFS2021_REMIND_NDC",
                   "MiddleEastAndAfrica",      "Power",     "NGFS2021_REMIND_NDC",
                   "MiddleEastAndAfrica",       "Coal",  "NGFS2021_REMIND_NZ2050",
                   "MiddleEastAndAfrica",    "Oil&Gas",  "NGFS2021_REMIND_NZ2050",
                   "MiddleEastAndAfrica",      "Power",  "NGFS2021_REMIND_NZ2050",
                              "Non-OECD",       "Coal",             "WEO2021_APS",
                              "Non-OECD",    "Oil&Gas",             "WEO2021_APS",
                              "Non-OECD",      "Power",             "WEO2021_APS",
                              "Non-OECD",       "Coal",             "WEO2021_SDS",
                              "Non-OECD",    "Oil&Gas",             "WEO2021_SDS",
                              "Non-OECD",      "Power",             "WEO2021_SDS",
                              "Non-OECD",       "Coal",           "WEO2021_STEPS",
                              "Non-OECD",    "Oil&Gas",           "WEO2021_STEPS",
                              "Non-OECD",      "Power",           "WEO2021_STEPS",
                          "NorthAmerica",       "Coal",             "WEO2021_APS",
                          "NorthAmerica",    "Oil&Gas",             "WEO2021_APS",
                          "NorthAmerica",      "Power",             "WEO2021_APS",
                          "NorthAmerica",       "Coal",             "WEO2021_SDS",
                          "NorthAmerica",    "Oil&Gas",             "WEO2021_SDS",
                          "NorthAmerica",      "Power",             "WEO2021_SDS",
                          "NorthAmerica",       "Coal",           "WEO2021_STEPS",
                          "NorthAmerica",    "Oil&Gas",           "WEO2021_STEPS",
                          "NorthAmerica",      "Power",           "WEO2021_STEPS",
                                  "OECD",       "Coal",             "WEO2021_APS",
                                  "OECD",    "Oil&Gas",             "WEO2021_APS",
                                  "OECD",      "Power",             "WEO2021_APS",
                                  "OECD",       "Coal",             "WEO2021_SDS",
                                  "OECD",    "Oil&Gas",             "WEO2021_SDS",
                                  "OECD",      "Power",             "WEO2021_SDS",
                                  "OECD",       "Coal",           "WEO2021_STEPS",
                                  "OECD",    "Oil&Gas",           "WEO2021_STEPS",
                                  "OECD",      "Power",           "WEO2021_STEPS",
                             "OecdAndEu",       "Coal",      "NGFS2021_GCAM_B2DS",
                             "OecdAndEu",    "Oil&Gas",      "NGFS2021_GCAM_B2DS",
                             "OecdAndEu",      "Power",      "NGFS2021_GCAM_B2DS",
                             "OecdAndEu",       "Coal",        "NGFS2021_GCAM_CP",
                             "OecdAndEu",    "Oil&Gas",        "NGFS2021_GCAM_CP",
                             "OecdAndEu",      "Power",        "NGFS2021_GCAM_CP",
                             "OecdAndEu",       "Coal",       "NGFS2021_GCAM_DN0",
                             "OecdAndEu",    "Oil&Gas",       "NGFS2021_GCAM_DN0",
                             "OecdAndEu",      "Power",       "NGFS2021_GCAM_DN0",
                             "OecdAndEu",       "Coal",        "NGFS2021_GCAM_DT",
                             "OecdAndEu",    "Oil&Gas",        "NGFS2021_GCAM_DT",
                             "OecdAndEu",      "Power",        "NGFS2021_GCAM_DT",
                             "OecdAndEu",       "Coal",       "NGFS2021_GCAM_NDC",
                             "OecdAndEu",    "Oil&Gas",       "NGFS2021_GCAM_NDC",
                             "OecdAndEu",      "Power",       "NGFS2021_GCAM_NDC",
                             "OecdAndEu",       "Coal",    "NGFS2021_GCAM_NZ2050",
                             "OecdAndEu",    "Oil&Gas",    "NGFS2021_GCAM_NZ2050",
                             "OecdAndEu",      "Power",    "NGFS2021_GCAM_NZ2050",
                             "OecdAndEu",       "Coal",   "NGFS2021_MESSAGE_B2DS",
                             "OecdAndEu",    "Oil&Gas",   "NGFS2021_MESSAGE_B2DS",
                             "OecdAndEu",      "Power",   "NGFS2021_MESSAGE_B2DS",
                             "OecdAndEu",       "Coal",     "NGFS2021_MESSAGE_CP",
                             "OecdAndEu",    "Oil&Gas",     "NGFS2021_MESSAGE_CP",
                             "OecdAndEu",      "Power",     "NGFS2021_MESSAGE_CP",
                             "OecdAndEu",       "Coal",    "NGFS2021_MESSAGE_DN0",
                             "OecdAndEu",    "Oil&Gas",    "NGFS2021_MESSAGE_DN0",
                             "OecdAndEu",      "Power",    "NGFS2021_MESSAGE_DN0",
                             "OecdAndEu",       "Coal",     "NGFS2021_MESSAGE_DT",
                             "OecdAndEu",    "Oil&Gas",     "NGFS2021_MESSAGE_DT",
                             "OecdAndEu",      "Power",     "NGFS2021_MESSAGE_DT",
                             "OecdAndEu",       "Coal",    "NGFS2021_MESSAGE_NDC",
                             "OecdAndEu",    "Oil&Gas",    "NGFS2021_MESSAGE_NDC",
                             "OecdAndEu",      "Power",    "NGFS2021_MESSAGE_NDC",
                             "OecdAndEu",       "Coal", "NGFS2021_MESSAGE_NZ2050",
                             "OecdAndEu",    "Oil&Gas", "NGFS2021_MESSAGE_NZ2050",
                             "OecdAndEu",      "Power", "NGFS2021_MESSAGE_NZ2050",
                             "OecdAndEu",       "Coal",    "NGFS2021_REMIND_B2DS",
                             "OecdAndEu",    "Oil&Gas",    "NGFS2021_REMIND_B2DS",
                             "OecdAndEu",      "Power",    "NGFS2021_REMIND_B2DS",
                             "OecdAndEu",       "Coal",      "NGFS2021_REMIND_CP",
                             "OecdAndEu",    "Oil&Gas",      "NGFS2021_REMIND_CP",
                             "OecdAndEu",      "Power",      "NGFS2021_REMIND_CP",
                             "OecdAndEu",       "Coal",     "NGFS2021_REMIND_DN0",
                             "OecdAndEu",    "Oil&Gas",     "NGFS2021_REMIND_DN0",
                             "OecdAndEu",      "Power",     "NGFS2021_REMIND_DN0",
                             "OecdAndEu",       "Coal",      "NGFS2021_REMIND_DT",
                             "OecdAndEu",    "Oil&Gas",      "NGFS2021_REMIND_DT",
                             "OecdAndEu",      "Power",      "NGFS2021_REMIND_DT",
                             "OecdAndEu",       "Coal",     "NGFS2021_REMIND_NDC",
                             "OecdAndEu",    "Oil&Gas",     "NGFS2021_REMIND_NDC",
                             "OecdAndEu",      "Power",     "NGFS2021_REMIND_NDC",
                             "OecdAndEu",       "Coal",  "NGFS2021_REMIND_NZ2050",
                             "OecdAndEu",    "Oil&Gas",  "NGFS2021_REMIND_NZ2050",
                             "OecdAndEu",      "Power",  "NGFS2021_REMIND_NZ2050",
                    "ReformingEconomies",       "Coal",      "NGFS2021_GCAM_B2DS",
                    "ReformingEconomies",    "Oil&Gas",      "NGFS2021_GCAM_B2DS",
                    "ReformingEconomies",      "Power",      "NGFS2021_GCAM_B2DS",
                    "ReformingEconomies",       "Coal",        "NGFS2021_GCAM_CP",
                    "ReformingEconomies",    "Oil&Gas",        "NGFS2021_GCAM_CP",
                    "ReformingEconomies",      "Power",        "NGFS2021_GCAM_CP",
                    "ReformingEconomies",       "Coal",       "NGFS2021_GCAM_DN0",
                    "ReformingEconomies",    "Oil&Gas",       "NGFS2021_GCAM_DN0",
                    "ReformingEconomies",      "Power",       "NGFS2021_GCAM_DN0",
                    "ReformingEconomies",       "Coal",        "NGFS2021_GCAM_DT",
                    "ReformingEconomies",    "Oil&Gas",        "NGFS2021_GCAM_DT",
                    "ReformingEconomies",      "Power",        "NGFS2021_GCAM_DT",
                    "ReformingEconomies",       "Coal",       "NGFS2021_GCAM_NDC",
                    "ReformingEconomies",    "Oil&Gas",       "NGFS2021_GCAM_NDC",
                    "ReformingEconomies",      "Power",       "NGFS2021_GCAM_NDC",
                    "ReformingEconomies",       "Coal",    "NGFS2021_GCAM_NZ2050",
                    "ReformingEconomies",    "Oil&Gas",    "NGFS2021_GCAM_NZ2050",
                    "ReformingEconomies",      "Power",    "NGFS2021_GCAM_NZ2050",
                    "ReformingEconomies",       "Coal",   "NGFS2021_MESSAGE_B2DS",
                    "ReformingEconomies",    "Oil&Gas",   "NGFS2021_MESSAGE_B2DS",
                    "ReformingEconomies",      "Power",   "NGFS2021_MESSAGE_B2DS",
                    "ReformingEconomies",       "Coal",     "NGFS2021_MESSAGE_CP",
                    "ReformingEconomies",    "Oil&Gas",     "NGFS2021_MESSAGE_CP",
                    "ReformingEconomies",      "Power",     "NGFS2021_MESSAGE_CP",
                    "ReformingEconomies",       "Coal",    "NGFS2021_MESSAGE_DN0",
                    "ReformingEconomies",    "Oil&Gas",    "NGFS2021_MESSAGE_DN0",
                    "ReformingEconomies",      "Power",    "NGFS2021_MESSAGE_DN0",
                    "ReformingEconomies",       "Coal",     "NGFS2021_MESSAGE_DT",
                    "ReformingEconomies",    "Oil&Gas",     "NGFS2021_MESSAGE_DT",
                    "ReformingEconomies",      "Power",     "NGFS2021_MESSAGE_DT",
                    "ReformingEconomies",       "Coal",    "NGFS2021_MESSAGE_NDC",
                    "ReformingEconomies",    "Oil&Gas",    "NGFS2021_MESSAGE_NDC",
                    "ReformingEconomies",      "Power",    "NGFS2021_MESSAGE_NDC",
                    "ReformingEconomies",       "Coal", "NGFS2021_MESSAGE_NZ2050",
                    "ReformingEconomies",    "Oil&Gas", "NGFS2021_MESSAGE_NZ2050",
                    "ReformingEconomies",      "Power", "NGFS2021_MESSAGE_NZ2050",
                    "ReformingEconomies",       "Coal",    "NGFS2021_REMIND_B2DS",
                    "ReformingEconomies",    "Oil&Gas",    "NGFS2021_REMIND_B2DS",
                    "ReformingEconomies",      "Power",    "NGFS2021_REMIND_B2DS",
                    "ReformingEconomies",       "Coal",      "NGFS2021_REMIND_CP",
                    "ReformingEconomies",    "Oil&Gas",      "NGFS2021_REMIND_CP",
                    "ReformingEconomies",      "Power",      "NGFS2021_REMIND_CP",
                    "ReformingEconomies",       "Coal",     "NGFS2021_REMIND_DN0",
                    "ReformingEconomies",    "Oil&Gas",     "NGFS2021_REMIND_DN0",
                    "ReformingEconomies",      "Power",     "NGFS2021_REMIND_DN0",
                    "ReformingEconomies",       "Coal",      "NGFS2021_REMIND_DT",
                    "ReformingEconomies",    "Oil&Gas",      "NGFS2021_REMIND_DT",
                    "ReformingEconomies",      "Power",      "NGFS2021_REMIND_DT",
                    "ReformingEconomies",       "Coal",     "NGFS2021_REMIND_NDC",
                    "ReformingEconomies",    "Oil&Gas",     "NGFS2021_REMIND_NDC",
                    "ReformingEconomies",      "Power",     "NGFS2021_REMIND_NDC",
                    "ReformingEconomies",       "Coal",  "NGFS2021_REMIND_NZ2050",
                    "ReformingEconomies",    "Oil&Gas",  "NGFS2021_REMIND_NZ2050",
                    "ReformingEconomies",      "Power",  "NGFS2021_REMIND_NZ2050",
                                "Russia",      "Power",             "IPR2021_FPS",
                                "Russia",      "Power",             "IPR2021_RPS",
                                "Russia",      "Power",        "IPR2021_baseline",
                                "Russia",      "Power",             "WEO2021_APS",
                                "Russia",      "Power",             "WEO2021_SDS",
                                "Russia",      "Power",           "WEO2021_STEPS",
                         "SoutheastAsia",       "Coal",      "NGFS2021_GCAM_B2DS",
                         "SoutheastAsia",    "Oil&Gas",      "NGFS2021_GCAM_B2DS",
                         "SoutheastAsia",      "Power",      "NGFS2021_GCAM_B2DS",
                         "SoutheastAsia",       "Coal",        "NGFS2021_GCAM_CP",
                         "SoutheastAsia",    "Oil&Gas",        "NGFS2021_GCAM_CP",
                         "SoutheastAsia",      "Power",        "NGFS2021_GCAM_CP",
                         "SoutheastAsia",       "Coal",       "NGFS2021_GCAM_DN0",
                         "SoutheastAsia",    "Oil&Gas",       "NGFS2021_GCAM_DN0",
                         "SoutheastAsia",      "Power",       "NGFS2021_GCAM_DN0",
                         "SoutheastAsia",       "Coal",        "NGFS2021_GCAM_DT",
                         "SoutheastAsia",    "Oil&Gas",        "NGFS2021_GCAM_DT",
                         "SoutheastAsia",      "Power",        "NGFS2021_GCAM_DT",
                         "SoutheastAsia",       "Coal",       "NGFS2021_GCAM_NDC",
                         "SoutheastAsia",    "Oil&Gas",       "NGFS2021_GCAM_NDC",
                         "SoutheastAsia",      "Power",       "NGFS2021_GCAM_NDC",
                         "SoutheastAsia",       "Coal",    "NGFS2021_GCAM_NZ2050",
                         "SoutheastAsia",    "Oil&Gas",    "NGFS2021_GCAM_NZ2050",
                         "SoutheastAsia",      "Power",    "NGFS2021_GCAM_NZ2050",
                          "UnitedStates",      "Power",             "IPR2021_FPS",
                          "UnitedStates",      "Power",             "IPR2021_RPS",
                          "UnitedStates",      "Power",        "IPR2021_baseline",
                          "UnitedStates",       "Coal",      "NGFS2021_GCAM_B2DS",
                          "UnitedStates",    "Oil&Gas",      "NGFS2021_GCAM_B2DS",
                          "UnitedStates",      "Power",      "NGFS2021_GCAM_B2DS",
                          "UnitedStates",       "Coal",        "NGFS2021_GCAM_CP",
                          "UnitedStates",    "Oil&Gas",        "NGFS2021_GCAM_CP",
                          "UnitedStates",      "Power",        "NGFS2021_GCAM_CP",
                          "UnitedStates",       "Coal",       "NGFS2021_GCAM_DN0",
                          "UnitedStates",    "Oil&Gas",       "NGFS2021_GCAM_DN0",
                          "UnitedStates",      "Power",       "NGFS2021_GCAM_DN0",
                          "UnitedStates",       "Coal",        "NGFS2021_GCAM_DT",
                          "UnitedStates",    "Oil&Gas",        "NGFS2021_GCAM_DT",
                          "UnitedStates",      "Power",        "NGFS2021_GCAM_DT",
                          "UnitedStates",       "Coal",       "NGFS2021_GCAM_NDC",
                          "UnitedStates",    "Oil&Gas",       "NGFS2021_GCAM_NDC",
                          "UnitedStates",      "Power",       "NGFS2021_GCAM_NDC",
                          "UnitedStates",       "Coal",    "NGFS2021_GCAM_NZ2050",
                          "UnitedStates",    "Oil&Gas",    "NGFS2021_GCAM_NZ2050",
                          "UnitedStates",      "Power",    "NGFS2021_GCAM_NZ2050",
                          "UnitedStates",       "Coal",    "NGFS2021_REMIND_B2DS",
                          "UnitedStates",    "Oil&Gas",    "NGFS2021_REMIND_B2DS",
                          "UnitedStates",      "Power",    "NGFS2021_REMIND_B2DS",
                          "UnitedStates",       "Coal",      "NGFS2021_REMIND_CP",
                          "UnitedStates",    "Oil&Gas",      "NGFS2021_REMIND_CP",
                          "UnitedStates",      "Power",      "NGFS2021_REMIND_CP",
                          "UnitedStates",       "Coal",     "NGFS2021_REMIND_DN0",
                          "UnitedStates",    "Oil&Gas",     "NGFS2021_REMIND_DN0",
                          "UnitedStates",      "Power",     "NGFS2021_REMIND_DN0",
                          "UnitedStates",       "Coal",      "NGFS2021_REMIND_DT",
                          "UnitedStates",    "Oil&Gas",      "NGFS2021_REMIND_DT",
                          "UnitedStates",      "Power",      "NGFS2021_REMIND_DT",
                          "UnitedStates",       "Coal",     "NGFS2021_REMIND_NDC",
                          "UnitedStates",    "Oil&Gas",     "NGFS2021_REMIND_NDC",
                          "UnitedStates",      "Power",     "NGFS2021_REMIND_NDC",
                          "UnitedStates",       "Coal",  "NGFS2021_REMIND_NZ2050",
                          "UnitedStates",    "Oil&Gas",  "NGFS2021_REMIND_NZ2050",
                          "UnitedStates",      "Power",  "NGFS2021_REMIND_NZ2050",
                          "UnitedStates",      "Power",             "WEO2021_APS",
                          "UnitedStates",      "Power",             "WEO2021_SDS",
                          "UnitedStates",      "Power",           "WEO2021_STEPS"
  )


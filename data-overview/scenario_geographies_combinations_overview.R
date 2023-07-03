library(tidyverse)
library(datapasta)
devtools::load_all()

# This document is a preparation for the whitelisting of the weo2021 and ngfs2021 data in the climate.stress.test.repo
# It first checks if sectors are complete
# It then filters distinct geography x sector combinations for each scenario
# Only those combinations that are present for all scenarios can stay
# Repeat process for capacity
# Join
# Done.

# Overview of supported scenarios x scenario-geographies x sectors --------
# Scenario_AnalysisInput_2021 ---------------------------------------------
Scenario_AnalysisInput_2021 <- readr::read_csv(
  file.path("data-raw", glue::glue("Scenarios_AnalysisInput_2021.csv"))
)

# first look at whether the sectors are complete
# if active in power, oil&gas and coal -> 9 technologies
# if active in  oil&gas and coal -> 3 technologies
# if active in  oil&gas and power -> 8 technologies
# if active only in power -> 6 technologies
# if active only in coal -> 1 technology
# if active only in oil&gas -> 2 technologies
Scenario_AnalysisInput_2021 %>%
  group_by(scenario_geography, scenario) %>%
  summarise(nrow = n(),
            n_sector = length(unique(ald_sector)),
            n_technologies = length(unique(technology)),
            sectors = list(unique(ald_sector)),
            technologies = list(unique(technology))) %>%
  dplyr::arrange(scenario_geography) %>%
  View()

#remove sector if not complete
p4i_p4b_sector_technology_lookup_df <- p4i_p4b_sector_technology_lookup()

Scenario_AnalysisInput_2021 <- Scenario_AnalysisInput_2021 %>%
  dplyr::filter(Scenario_AnalysisInput_2021$ald_sector %in% unique(p4i_p4b_sector_technology_lookup_df$sector_p4i))

Scenario_AnalysisInput_2021 <- remove_incomplete_sectors(Scenario_AnalysisInput_2021)


# we can only include geographies that are present both in baseline (currently APS, STEPS, GEO ref )
# and shock scenario (currently SDS, nze250)
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

# GECO baseline (CurPol) and shock (_1.5c, NDC-LTS)
Scenario_AnalysisInput_2021_GECO2021_CurPol <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("GECO2021_CurPol")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_GECO2021_NDC_LTS <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("GECO2021_NDC-LTS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_GECO2021_1.5c <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("GECO2021_1.5C-Unif")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

# NGFS basline(NDC,CP) vs shock (B2DS, DN0, DT, NDC, NZ2050)
Scenario_AnalysisInput_2021_GCAM_B2DS <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_GCAM_B2DS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_GCAM_CP <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_GCAM_CP")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_GCAM_DN0 <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_GCAM_DN0")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_GCAM_DT <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_GCAM_DT")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_GCAM_NDC <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_GCAM_NDC")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_GCAM_NZ2050 <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_GCAM_NZ2050")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_REMIND_B2DS <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_REMIND_B2DS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_REMIND_CP <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_REMIND_CP")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_REMIND_DN0 <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_REMIND_DN0")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_REMIND_DT <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_REMIND_DT")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_REMIND_NDC <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_REMIND_NDC")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_REMIND_NZ2050 <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_REMIND_NZ2050")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_MESSAGE_B2DS <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_B2DS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_MESSAGE_CP <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_CP")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_MESSAGE_DT <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_DT")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_MESSAGE_DN0 <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_DN0")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_MESSAGE_NDC <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_NDC")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_MESSAGE_NZ2050 <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("NGFS2021_MESSAGE_NZ2050")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

### Oxford
Scenario_AnalysisInput_2021_Oxford_base <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("Oxford2021_base")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_Oxford_fast <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("Oxford2021_fast")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_oxford <- Scenario_AnalysisInput_2021_Oxford_base %>%
  select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_Oxford_fast %>%
               select(scenario_geography, ald_sector))

#Scenario_AnalysisInput_2021_oxford <- Scenario_AnalysisInput_2021_oxford %>% tribble_paste()

tibble::tribble(
  ~scenario_geography, ~ald_sector,
             "Global",     "Power",
             "Global",      "Coal",
             "Global",   "Oil&Gas"
  )


### IPR (baselin vd FPS and RPS)
Scenario_AnalysisInput_2021_IPR_FPS <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("IPR2021_FPS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_IPR_RPS <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("IPR2021_RPS")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()

Scenario_AnalysisInput_2021_IPR_baseline <- Scenario_AnalysisInput_2021 %>%
  filter(scenario %in% c("IPR2021_baseline")) %>%
  select(scenario, scenario_geography, ald_sector) %>%
  distinct_all()


##IPR Baseline (IEA SPS vs Shock IPR RPS/FPS)
Scenario_AnalysisInput_2021_ipr <- Scenario_AnalysisInput_2021_IPR_FPS %>%
  select(scenario_geography, ald_sector)%>%
  inner_join(Scenario_AnalysisInput_2021_IPR_RPS %>%
               select(scenario_geography, ald_sector) %>%
               inner_join(Scenario_AnalysisInput_2021_IPR_baseline)) %>%
  select(scenario_geography, ald_sector)


#Scenario_AnalysisInput_2021_ipr <- Scenario_AnalysisInput_2021_ipr %>% tribble_paste()

tibble::tribble(
  ~scenario_geography, ~ald_sector,
             "Global",      "Coal",
             "Global",   "Oil&Gas",
             "Global",     "Power"
  )

# NGFS basline(NDC,CP) vs shock
Scenario_AnalysisInput_2021_ngfs <- Scenario_AnalysisInput_2021_GCAM_B2DS %>%
  select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_GCAM_CP %>%
               select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_GCAM_DN0 %>%
               select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_GCAM_DT %>%
               select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_GCAM_NDC %>%
               select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_GCAM_NZ2050 %>%
               select(scenario_geography, ald_sector)  %>%
  inner_join(Scenario_AnalysisInput_2021_REMIND_B2DS %>%
               select(scenario_geography, ald_sector)  %>%
  inner_join(Scenario_AnalysisInput_2021_REMIND_B2DS %>%
               select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_REMIND_CP %>%
               select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_REMIND_DN0 %>%
             select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_REMIND_DT %>%
             select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_REMIND_NDC %>%
             select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_REMIND_NZ2050 %>%
             select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_MESSAGE_B2DS %>%
             select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_MESSAGE_CP %>%
             select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_MESSAGE_DN0 %>%
             select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_MESSAGE_DT %>%
             select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_MESSAGE_NDC %>%
             select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_MESSAGE_NZ2050)))))))))))))))))) %>%
              select(scenario_geography, ald_sector)

#Scenario_AnalysisInput_2021_ngfs <- Scenario_AnalysisInput_2021_ngfs %>% tribble_paste()
tibble::tribble(
          ~scenario_geography, ~ald_sector,
                  "Asia (R5)",      "Coal",
                  "Asia (R5)",   "Oil&Gas",
                  "Asia (R5)",     "Power",
                     "Global",      "Coal",
                     "Global",   "Oil&Gas",
                     "Global",     "Power",
         "Latin America (R5)",      "Coal",
         "Latin America (R5)",   "Oil&Gas",
         "Latin America (R5)",     "Power",
  "Middle East & Africa (R5)",      "Coal",
  "Middle East & Africa (R5)",   "Oil&Gas",
  "Middle East & Africa (R5)",     "Power",
             "OECD & EU (R5)",      "Coal",
             "OECD & EU (R5)",   "Oil&Gas",
             "OECD & EU (R5)",     "Power",
   "Reforming Economies (R5)",      "Coal",
   "Reforming Economies (R5)",   "Oil&Gas",
   "Reforming Economies (R5)",     "Power"
  )



# binding GECO scenario together add later to overlap_all df
Scenario_AnalysisInput_2021_Geco <- rbind(Scenario_AnalysisInput_2021_GECO2021_CurPol, Scenario_AnalysisInput_2021_GECO2021_NDC_LTS, Scenario_AnalysisInput_2021_GECO2021_1.5c)

#binding WEO together
Scenario_AnalysisInput_2021_without_nze <- Scenario_AnalysisInput_2021_APS %>%
  select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_SDS %>%
    select(scenario_geography, ald_sector) %>%
    inner_join(Scenario_AnalysisInput_2021_STEPS) %>%
    select(scenario_geography, ald_sector)) %>%
  arrange(scenario_geography)


#Scenario_AnalysisInput_2021_without_nze <- Scenario_AnalysisInput_2021_without_nze %>% tribble_paste()
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
                                  "EU",     "Power",
  "Emergingmarket&developingeconomies",      "Coal",
  "Emergingmarket&developingeconomies",   "Oil&Gas",
  "Emergingmarket&developingeconomies",     "Power",
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


Scenario_AnalysisInput_2021_scenarios <- Scenario_AnalysisInput_2021_APS %>%
  select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_SDS) %>%
  select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_NZE_2050) %>%
  select(scenario_geography, ald_sector) %>%
  inner_join(Scenario_AnalysisInput_2021_STEPS) %>%
  select(scenario_geography, ald_sector) %>%
  arrange(scenario_geography)

# Scenario_AnalysisInput_2021_scenarios <- Scenario_AnalysisInput_2021_scenarios %>% tribble_paste()
tibble::tribble(
  ~scenario_geography, ~ald_sector,
             "Global",      "Coal",
             "Global",   "Oil&Gas",
             "Global",     "Power"
  )



# prewrangled_capacity_factors --------------------------------------------
# NOTE: Only relevant for power sector

# prewrangled_capacity_factors
# using prepared_data as prewrangled_capacity_data is outdated address!-----------------------------------
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


#NGFS
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

prewrangled_capacity_factors_ngfs_scenarios <- prewrangled_capacity_factors_MESSAGE_B2DS %>%
  select(scenario_geography) %>%
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
  inner_join(prewrangled_capacity_factors_REMIND_B2DS %>%
               select(scenario_geography)) %>%
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
               inner_join(prewrangled_capacity_factors_GCAM_B2DS %>%
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
               select(scenario_geography)) %>%
  arrange(scenario_geography)

#prewrangled_capacity_factors_ngfs_scenarios %>% tribble_paste()
tibble::tribble(
          ~scenario_geography,
                  "Asia (R5)",
                     "Global",
         "Latin America (R5)",
  "Middle East & Africa (R5)",
             "OECD & EU (R5)",
   "Reforming Economies (R5)"
  )


### Oxford
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

#prewrangled_capacity_factors_oxford_scenarios %>% tribble_paste()

tibble::tribble(
  ~scenario_geography,
             "Global"
  )


### IPR
# we can only include geographies that are present both in baseline
# and shock scenario (IPR FPS and IPR RPS)
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
             "Global"
  )

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

##do, as i believe now, scenario geographies that are whitelisted need to be present in production data
abcd_stress_test_input <- r2dii.utils::path_dropbox_2dii("ST_INPUTS", "ST_INPUTS_MASTER", "abcd_stress_test_input.csv")

abcd_stress_test_input <- readr::read_csv(
  abcd_stress_test_input)

abcd_stress_test_geographies <- abcd_stress_test_input %>%
  select(scenario_geography) %>%
  distinct_all()

tibble::tribble(
                   ~scenario_geography,
                           "Asia (R5)",
                         "AsiaPacific",
  "Emergingmarket&developingeconomies",
                              "Global",
                                 "IDN",
                                 "IND",
                               "India",
                             "NonOECD",
                                 "AUS",
                   "AdvancedEconomies",
                                 "BRA",
                              "Brazil",
                  "Latin America (R5)",
                        "LatinAmerica",
                                "OECD",
                      "OECD & EU (R5)",
                        "NorthAmerica",
                                  "US",
                                 "USA",
                                  "EU",
                              "Europe",
                                 "WEU",
                              "Africa",
           "Middle East & Africa (R5)",
                                 "ZAF",
                                "EURA",
                             "Eurasia",
                                "MENA",
            "Reforming Economies (R5)",
                                 "CAN",
                                 "GBR",
                                 "GCC",
                          "MiddleEast",
                                 "JPN",
                               "Japan",
                                 "CSA",
                                "SEAO",
                                 "SSA",
                                 "EEU",
                                  "SA",
                                 "RUS",
                              "Russia",
                                 "CHN",
                               "China",
                                 "KOR"
  )



# Supported overlap -------------------------------------------------------
# We can only offer scenario_geography x region combinations for which baseline and
# shock results are available in all relevant dataset.
# This overlap is pointed here for datasets as currently used for ST user workflow

# Harmonizing naming conventions to P4I standard
cap_fac_harmonised <- prewrangled_capacity_factors_WEO_2021_scenarios %>%
  dplyr::mutate(scenario_geography = gsub(" ", "", scenario_geography, fixed = TRUE)) %>%
  dplyr::mutate(scenario_geography = case_when(
    scenario_geography == "EuropeanUnion" ~ "EU",
    scenario_geography == "Non-OECD" ~ "NonOECD",
    scenario_geography == "UnitedStates" ~ "US",
    TRUE ~ scenario_geography
  ))

Scenario_AnalysisInput_2021_without_nze_harmonized <- Scenario_AnalysisInput_2021_without_nze %>%
  dplyr::mutate(scenario_geography = case_when(
    scenario_geography == "EU27" ~ "EU",
    scenario_geography == "Emerging market and developing economies" ~ "Emergingmarket&developingeconomies",
    scenario_geography == "Advanced economies" ~ "AdvancedEconomies",
    TRUE ~ scenario_geography
  ))


# removing scenario_geography x Power combinations that are missing in cap_fac_harmonised

overlap_all <- Scenario_AnalysisInput_2021_without_nze_harmonized %>%
  filter(!(ald_sector == "Power" & !.data$scenario_geography %in% cap_fac_harmonised$scenario_geography))

## excluded is the power sector in Latin america as not present in capacity factors
## we also drop the power sector DevelopingEconomis as this category is not present in ScenarioAnalysisInput -> Question: we could rename to Emergingmarket&developingeconomies as this is present in the scenario analysis
excluded <- setdiff(Scenario_AnalysisInput_2021_without_nze_harmonized, overlap_all)

## geographies overlap with
overlap_all <- overlap_all %>% inner_join(abcd_stress_test_geographies)


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
                                  "EU",     "Power",
  "Emergingmarket&developingeconomies",      "Coal",
  "Emergingmarket&developingeconomies",   "Oil&Gas",
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



overlap_all$scenario_STEPS <- "WEO2021_STEPS"
overlap_all$scenario_SDS <- "WEO2021_SDS"
overlap_all$scenario_APS <- "WEO2021_APS"

overlap_all <- overlap_all %>%
  pivot_longer(scenario_STEPS:scenario_APS, values_to = "scenario") %>%
  select(-c(name))

overlap_all <- overlap_all %>% arrange(scenario_geography, scenario)

overlap_all <- overlap_all %>% full_join(Scenario_AnalysisInput_2021_Geco)

overlap_all <- overlap_all %>% full_join(Scenario_AnalysisInput_2021_NZE_2050)

overlap_all <- overlap_all %>% arrange(scenario_geography, scenario)

tibble::tribble(
                   ~scenario_geography,  ~ald_sector,            ~scenario,
                   "AdvancedEconomies",       "Coal",        "WEO2021_APS",
                   "AdvancedEconomies",    "Oil&Gas",        "WEO2021_APS",
                   "AdvancedEconomies",      "Power",        "WEO2021_APS",
                   "AdvancedEconomies",       "Coal",        "WEO2021_SDS",
                   "AdvancedEconomies",    "Oil&Gas",        "WEO2021_SDS",
                   "AdvancedEconomies",      "Power",        "WEO2021_SDS",
                   "AdvancedEconomies",       "Coal",      "WEO2021_STEPS",
                   "AdvancedEconomies",    "Oil&Gas",      "WEO2021_STEPS",
                   "AdvancedEconomies",      "Power",      "WEO2021_STEPS",
                              "Africa",       "Coal",        "WEO2021_APS",
                              "Africa",    "Oil&Gas",        "WEO2021_APS",
                              "Africa",      "Power",        "WEO2021_APS",
                              "Africa",       "Coal",        "WEO2021_SDS",
                              "Africa",    "Oil&Gas",        "WEO2021_SDS",
                              "Africa",      "Power",        "WEO2021_SDS",
                              "Africa",       "Coal",      "WEO2021_STEPS",
                              "Africa",    "Oil&Gas",      "WEO2021_STEPS",
                              "Africa",      "Power",      "WEO2021_STEPS",
                         "AsiaPacific",       "Coal",        "WEO2021_APS",
                         "AsiaPacific",    "Oil&Gas",        "WEO2021_APS",
                         "AsiaPacific",      "Power",        "WEO2021_APS",
                         "AsiaPacific",       "Coal",        "WEO2021_SDS",
                         "AsiaPacific",    "Oil&Gas",        "WEO2021_SDS",
                         "AsiaPacific",      "Power",        "WEO2021_SDS",
                         "AsiaPacific",       "Coal",      "WEO2021_STEPS",
                         "AsiaPacific",    "Oil&Gas",      "WEO2021_STEPS",
                         "AsiaPacific",      "Power",      "WEO2021_STEPS",
                              "Brazil",      "Power",        "WEO2021_APS",
                              "Brazil",      "Power",        "WEO2021_SDS",
                              "Brazil",      "Power",      "WEO2021_STEPS",
                               "China",      "Power",        "WEO2021_APS",
                               "China",      "Power",        "WEO2021_SDS",
                               "China",      "Power",      "WEO2021_STEPS",
                                  "EU",      "Power",        "WEO2021_APS",
                                  "EU",      "Power",        "WEO2021_SDS",
                                  "EU",      "Power",      "WEO2021_STEPS",
  "Emergingmarket&developingeconomies",       "Coal",        "WEO2021_APS",
  "Emergingmarket&developingeconomies",    "Oil&Gas",        "WEO2021_APS",
  "Emergingmarket&developingeconomies",       "Coal",        "WEO2021_SDS",
  "Emergingmarket&developingeconomies",    "Oil&Gas",        "WEO2021_SDS",
  "Emergingmarket&developingeconomies",       "Coal",      "WEO2021_STEPS",
  "Emergingmarket&developingeconomies",    "Oil&Gas",      "WEO2021_STEPS",
                             "Eurasia",       "Coal",        "WEO2021_APS",
                             "Eurasia",    "Oil&Gas",        "WEO2021_APS",
                             "Eurasia",      "Power",        "WEO2021_APS",
                             "Eurasia",       "Coal",        "WEO2021_SDS",
                             "Eurasia",    "Oil&Gas",        "WEO2021_SDS",
                             "Eurasia",      "Power",        "WEO2021_SDS",
                             "Eurasia",       "Coal",      "WEO2021_STEPS",
                             "Eurasia",    "Oil&Gas",      "WEO2021_STEPS",
                             "Eurasia",      "Power",      "WEO2021_STEPS",
                              "Europe",       "Coal",        "WEO2021_APS",
                              "Europe",    "Oil&Gas",        "WEO2021_APS",
                              "Europe",      "Power",        "WEO2021_APS",
                              "Europe",       "Coal",        "WEO2021_SDS",
                              "Europe",    "Oil&Gas",        "WEO2021_SDS",
                              "Europe",      "Power",        "WEO2021_SDS",
                              "Europe",       "Coal",      "WEO2021_STEPS",
                              "Europe",    "Oil&Gas",      "WEO2021_STEPS",
                              "Europe",      "Power",      "WEO2021_STEPS",
                              "Global", "Automotive", "GECO2021_1.5C-Unif",
                              "Global", "Automotive",    "GECO2021_CurPol",
                              "Global", "Automotive",   "GECO2021_NDC-LTS",
                              "Global",       "Coal",        "WEO2021_APS",
                              "Global",    "Oil&Gas",        "WEO2021_APS",
                              "Global",      "Power",        "WEO2021_APS",
                              "Global", "Automotive",   "WEO2021_NZE_2050",
                              "Global",       "Coal",   "WEO2021_NZE_2050",
                              "Global",    "Oil&Gas",   "WEO2021_NZE_2050",
                              "Global",      "Power",   "WEO2021_NZE_2050",
                              "Global",       "Coal",        "WEO2021_SDS",
                              "Global",    "Oil&Gas",        "WEO2021_SDS",
                              "Global",      "Power",        "WEO2021_SDS",
                              "Global",       "Coal",      "WEO2021_STEPS",
                              "Global",    "Oil&Gas",      "WEO2021_STEPS",
                              "Global",      "Power",      "WEO2021_STEPS",
                               "India",      "Power",        "WEO2021_APS",
                               "India",      "Power",        "WEO2021_SDS",
                               "India",      "Power",      "WEO2021_STEPS",
                               "Japan",      "Power",        "WEO2021_APS",
                               "Japan",      "Power",        "WEO2021_SDS",
                               "Japan",      "Power",      "WEO2021_STEPS",
                        "LatinAmerica",       "Coal",        "WEO2021_APS",
                        "LatinAmerica",    "Oil&Gas",        "WEO2021_APS",
                        "LatinAmerica",       "Coal",        "WEO2021_SDS",
                        "LatinAmerica",    "Oil&Gas",        "WEO2021_SDS",
                        "LatinAmerica",       "Coal",      "WEO2021_STEPS",
                        "LatinAmerica",    "Oil&Gas",      "WEO2021_STEPS",
                          "MiddleEast",       "Coal",        "WEO2021_APS",
                          "MiddleEast",    "Oil&Gas",        "WEO2021_APS",
                          "MiddleEast",      "Power",        "WEO2021_APS",
                          "MiddleEast",       "Coal",        "WEO2021_SDS",
                          "MiddleEast",    "Oil&Gas",        "WEO2021_SDS",
                          "MiddleEast",      "Power",        "WEO2021_SDS",
                          "MiddleEast",       "Coal",      "WEO2021_STEPS",
                          "MiddleEast",    "Oil&Gas",      "WEO2021_STEPS",
                          "MiddleEast",      "Power",      "WEO2021_STEPS",
                             "NonOECD",       "Coal",        "WEO2021_APS",
                             "NonOECD",    "Oil&Gas",        "WEO2021_APS",
                             "NonOECD",      "Power",        "WEO2021_APS",
                             "NonOECD",       "Coal",        "WEO2021_SDS",
                             "NonOECD",    "Oil&Gas",        "WEO2021_SDS",
                             "NonOECD",      "Power",        "WEO2021_SDS",
                             "NonOECD",       "Coal",      "WEO2021_STEPS",
                             "NonOECD",    "Oil&Gas",      "WEO2021_STEPS",
                             "NonOECD",      "Power",      "WEO2021_STEPS",
                        "NorthAmerica",       "Coal",        "WEO2021_APS",
                        "NorthAmerica",    "Oil&Gas",        "WEO2021_APS",
                        "NorthAmerica",      "Power",        "WEO2021_APS",
                        "NorthAmerica",       "Coal",        "WEO2021_SDS",
                        "NorthAmerica",    "Oil&Gas",        "WEO2021_SDS",
                        "NorthAmerica",      "Power",        "WEO2021_SDS",
                        "NorthAmerica",       "Coal",      "WEO2021_STEPS",
                        "NorthAmerica",    "Oil&Gas",      "WEO2021_STEPS",
                        "NorthAmerica",      "Power",      "WEO2021_STEPS",
                                "OECD",       "Coal",        "WEO2021_APS",
                                "OECD",    "Oil&Gas",        "WEO2021_APS",
                                "OECD",      "Power",        "WEO2021_APS",
                                "OECD",       "Coal",        "WEO2021_SDS",
                                "OECD",    "Oil&Gas",        "WEO2021_SDS",
                                "OECD",      "Power",        "WEO2021_SDS",
                                "OECD",       "Coal",      "WEO2021_STEPS",
                                "OECD",    "Oil&Gas",      "WEO2021_STEPS",
                                "OECD",      "Power",      "WEO2021_STEPS",
                              "Russia",      "Power",        "WEO2021_APS",
                              "Russia",      "Power",        "WEO2021_SDS",
                              "Russia",      "Power",      "WEO2021_STEPS",
                                  "US",      "Power",        "WEO2021_APS",
                                  "US",      "Power",        "WEO2021_SDS",
                                  "US",      "Power",      "WEO2021_STEPS"
  )



## overlap with capacity factors
overlap_all_ngfs <- Scenario_AnalysisInput_2021_ngfs %>%
  filter(!(ald_sector == "Power" & !.data$scenario_geography %in% prewrangled_capacity_factors_ngfs_scenarios$scenario_geography))

## geographies overlap with production data ##this is problematic here
overlap_all_ngfs <- overlap_all_ngfs %>% inner_join(abcd_stress_test_geographies)

overlap_all_ngfs$scenario_GCAM_B2DS <- "NGFS2021_GCAM_B2DS"
overlap_all_ngfs$scenario_GCAM_CP <-"NGFS2021_GCAM_CP"
overlap_all_ngfs$scenario_GCAM_DN0 <- "NGFS2021_GCAM_DN0"
overlap_all_ngfs$scenario_GCAM_DT  <- "NGFS2021_GCAM_DT"
overlap_all_ngfs$scenario_GCAM_NDC  <-"NGFS2021_GCAM_NDC"
overlap_all_ngfs$scenario_GCAM_NZ2050  <-"NGFS2021_GCAM_NZ2050"
overlap_all_ngfs$scenario_REMIND_B2DS <- "NGFS2021_REMIND_B2DS"
overlap_all_ngfs$scenario_REMIND_CP <-"NGFS2021_REMIND_CP"
overlap_all_ngfs$scenario_REMIND_DN0 <- "NGFS2021_REMIND_DN0"
overlap_all_ngfs$scenario_REMIND_DT  <- "NGFS2021_REMIND_DT"
overlap_all_ngfs$scenario_REMIND_NDC  <-"NGFS2021_REMIND_NDC"
overlap_all_ngfs$scenario_REMIND_NZ2050  <-"NGFS2021_REMIND_NZ2050"
overlap_all_ngfs$scenario_MESSAGE_B2DS <- "NGFS2021_MESSAGE_B2DS"
overlap_all_ngfs$scenario_MESSAGE_CP <-"NGFS2021_MESSAGE_CP"
overlap_all_ngfs$scenario_MESSAGE_DN0 <- "NGFS2021_MESSAGE_DN0"
overlap_all_ngfs$scenario_MESSAGE_DT  <- "NGFS2021_MESSAGE_DT"
overlap_all_ngfs$scenario_MESSAGE_NDC  <-"NGFS2021_MESSAGE_NDC"
overlap_all_ngfs$scenario_MESSAGE_NZ2050  <-"NGFS2021_MESSAGE_NZ2050"

overlap_all_ngfs <- overlap_all_ngfs %>%
  pivot_longer(scenario_GCAM_B2DS:scenario_MESSAGE_NZ2050, values_to = "scenario") %>%
  select(-c(name))

overlap_all_ngfs <- overlap_all_ngfs %>% arrange(scenario_geography, scenario)

#### Oxford
## overlap with IPR capacity factors
overlap_all_oxford <- Scenario_AnalysisInput_2021_oxford %>%
  filter(!(ald_sector == "Power" & !.data$scenario_geography %in% prewrangled_capacity_factors_oxford_scenarios$scenario_geography))

## geographies overlap with production data
overlap_all_oxford <- overlap_all_oxford %>% inner_join(abcd_stress_test_geographies)

overlap_all_oxford$scenario_fast <- "Oxford2021_fast"
overlap_all_oxford$scenario_base <-"Oxford2021_base"


overlap_all_oxford <- overlap_all_oxford %>%
  pivot_longer(scenario_fast:scenario_base, values_to = "scenario") %>%
  select(-c(name))

overlap_all_oxford <- overlap_all_oxford %>% arrange(scenario_geography, scenario)

####IPR
## overlap with IPR capacity factors
overlap_all_ipr <- Scenario_AnalysisInput_2021_ipr %>%
  filter(!(ald_sector == "Power" & !.data$scenario_geography %in% prewrangled_capacity_factors_ipr_scenarios$scenario_geography))


## geographies overlap with production data
overlap_all_ipr <- overlap_all_ipr %>% inner_join(abcd_stress_test_geographies)

overlap_all_ipr$scenario_FPS <- "IPR2021_FPS"
overlap_all_ipr$scenario_RPS <-"IPR2021_RPS"
overlap_all_ipr$scenario_baseline <-"IPR2021_baseline"



overlap_all_ipr <- overlap_all_ipr %>%
  pivot_longer(scenario_FPS:scenario_baseline, values_to = "scenario") %>%
  select(-c(name))

overlap_all_ipr <- overlap_all_ipr %>% arrange(scenario_geography, scenario)

## joining all scenarios
overlap_all_combined <- full_join(overlap_all, overlap_all_ngfs) %>% arrange(scenario_geography, scenario)
overlap_all_combined <- full_join(overlap_all_combined, overlap_all_ipr) %>% arrange(scenario_geography, scenario)
overlap_all_combined <- full_join(overlap_all_combined, overlap_all_oxford) %>% arrange(scenario_geography, scenario)

#overlap_all_combined <- overlap_all_combined %>% tribble_paste()
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
                           "Asia (R5)",       "Coal",      "NGFS2021_GCAM_B2DS",
                           "Asia (R5)",    "Oil&Gas",      "NGFS2021_GCAM_B2DS",
                           "Asia (R5)",      "Power",      "NGFS2021_GCAM_B2DS",
                           "Asia (R5)",       "Coal",        "NGFS2021_GCAM_CP",
                           "Asia (R5)",    "Oil&Gas",        "NGFS2021_GCAM_CP",
                           "Asia (R5)",      "Power",        "NGFS2021_GCAM_CP",
                           "Asia (R5)",       "Coal",       "NGFS2021_GCAM_DN0",
                           "Asia (R5)",    "Oil&Gas",       "NGFS2021_GCAM_DN0",
                           "Asia (R5)",      "Power",       "NGFS2021_GCAM_DN0",
                           "Asia (R5)",       "Coal",        "NGFS2021_GCAM_DT",
                           "Asia (R5)",    "Oil&Gas",        "NGFS2021_GCAM_DT",
                           "Asia (R5)",      "Power",        "NGFS2021_GCAM_DT",
                           "Asia (R5)",       "Coal",       "NGFS2021_GCAM_NDC",
                           "Asia (R5)",    "Oil&Gas",       "NGFS2021_GCAM_NDC",
                           "Asia (R5)",      "Power",       "NGFS2021_GCAM_NDC",
                           "Asia (R5)",       "Coal",    "NGFS2021_GCAM_NZ2050",
                           "Asia (R5)",    "Oil&Gas",    "NGFS2021_GCAM_NZ2050",
                           "Asia (R5)",      "Power",    "NGFS2021_GCAM_NZ2050",
                           "Asia (R5)",       "Coal",   "NGFS2021_MESSAGE_B2DS",
                           "Asia (R5)",    "Oil&Gas",   "NGFS2021_MESSAGE_B2DS",
                           "Asia (R5)",      "Power",   "NGFS2021_MESSAGE_B2DS",
                           "Asia (R5)",       "Coal",     "NGFS2021_MESSAGE_CP",
                           "Asia (R5)",    "Oil&Gas",     "NGFS2021_MESSAGE_CP",
                           "Asia (R5)",      "Power",     "NGFS2021_MESSAGE_CP",
                           "Asia (R5)",       "Coal",    "NGFS2021_MESSAGE_DN0",
                           "Asia (R5)",    "Oil&Gas",    "NGFS2021_MESSAGE_DN0",
                           "Asia (R5)",      "Power",    "NGFS2021_MESSAGE_DN0",
                           "Asia (R5)",       "Coal",     "NGFS2021_MESSAGE_DT",
                           "Asia (R5)",    "Oil&Gas",     "NGFS2021_MESSAGE_DT",
                           "Asia (R5)",      "Power",     "NGFS2021_MESSAGE_DT",
                           "Asia (R5)",       "Coal",    "NGFS2021_MESSAGE_NDC",
                           "Asia (R5)",    "Oil&Gas",    "NGFS2021_MESSAGE_NDC",
                           "Asia (R5)",      "Power",    "NGFS2021_MESSAGE_NDC",
                           "Asia (R5)",       "Coal", "NGFS2021_MESSAGE_NZ2050",
                           "Asia (R5)",    "Oil&Gas", "NGFS2021_MESSAGE_NZ2050",
                           "Asia (R5)",      "Power", "NGFS2021_MESSAGE_NZ2050",
                           "Asia (R5)",       "Coal",    "NGFS2021_REMIND_B2DS",
                           "Asia (R5)",    "Oil&Gas",    "NGFS2021_REMIND_B2DS",
                           "Asia (R5)",      "Power",    "NGFS2021_REMIND_B2DS",
                           "Asia (R5)",       "Coal",      "NGFS2021_REMIND_CP",
                           "Asia (R5)",    "Oil&Gas",      "NGFS2021_REMIND_CP",
                           "Asia (R5)",      "Power",      "NGFS2021_REMIND_CP",
                           "Asia (R5)",       "Coal",     "NGFS2021_REMIND_DN0",
                           "Asia (R5)",    "Oil&Gas",     "NGFS2021_REMIND_DN0",
                           "Asia (R5)",      "Power",     "NGFS2021_REMIND_DN0",
                           "Asia (R5)",       "Coal",      "NGFS2021_REMIND_DT",
                           "Asia (R5)",    "Oil&Gas",      "NGFS2021_REMIND_DT",
                           "Asia (R5)",      "Power",      "NGFS2021_REMIND_DT",
                           "Asia (R5)",       "Coal",     "NGFS2021_REMIND_NDC",
                           "Asia (R5)",    "Oil&Gas",     "NGFS2021_REMIND_NDC",
                           "Asia (R5)",      "Power",     "NGFS2021_REMIND_NDC",
                           "Asia (R5)",       "Coal",  "NGFS2021_REMIND_NZ2050",
                           "Asia (R5)",    "Oil&Gas",  "NGFS2021_REMIND_NZ2050",
                           "Asia (R5)",      "Power",  "NGFS2021_REMIND_NZ2050",
                         "AsiaPacific",       "Coal",             "WEO2021_APS",
                         "AsiaPacific",    "Oil&Gas",             "WEO2021_APS",
                         "AsiaPacific",      "Power",             "WEO2021_APS",
                         "AsiaPacific",       "Coal",             "WEO2021_SDS",
                         "AsiaPacific",    "Oil&Gas",             "WEO2021_SDS",
                         "AsiaPacific",      "Power",             "WEO2021_SDS",
                         "AsiaPacific",       "Coal",           "WEO2021_STEPS",
                         "AsiaPacific",    "Oil&Gas",           "WEO2021_STEPS",
                         "AsiaPacific",      "Power",           "WEO2021_STEPS",
                              "Brazil",      "Power",             "WEO2021_APS",
                              "Brazil",      "Power",             "WEO2021_SDS",
                              "Brazil",      "Power",           "WEO2021_STEPS",
                               "China",      "Power",             "WEO2021_APS",
                               "China",      "Power",             "WEO2021_SDS",
                               "China",      "Power",           "WEO2021_STEPS",
                                  "EU",      "Power",             "WEO2021_APS",
                                  "EU",      "Power",             "WEO2021_SDS",
                                  "EU",      "Power",           "WEO2021_STEPS",
  "Emergingmarket&developingeconomies",       "Coal",             "WEO2021_APS",
  "Emergingmarket&developingeconomies",    "Oil&Gas",             "WEO2021_APS",
  "Emergingmarket&developingeconomies",       "Coal",             "WEO2021_SDS",
  "Emergingmarket&developingeconomies",    "Oil&Gas",             "WEO2021_SDS",
  "Emergingmarket&developingeconomies",       "Coal",           "WEO2021_STEPS",
  "Emergingmarket&developingeconomies",    "Oil&Gas",           "WEO2021_STEPS",
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
                               "India",      "Power",             "WEO2021_APS",
                               "India",      "Power",             "WEO2021_SDS",
                               "India",      "Power",           "WEO2021_STEPS",
                               "Japan",      "Power",             "WEO2021_APS",
                               "Japan",      "Power",             "WEO2021_SDS",
                               "Japan",      "Power",           "WEO2021_STEPS",
                  "Latin America (R5)",       "Coal",      "NGFS2021_GCAM_B2DS",
                  "Latin America (R5)",    "Oil&Gas",      "NGFS2021_GCAM_B2DS",
                  "Latin America (R5)",      "Power",      "NGFS2021_GCAM_B2DS",
                  "Latin America (R5)",       "Coal",        "NGFS2021_GCAM_CP",
                  "Latin America (R5)",    "Oil&Gas",        "NGFS2021_GCAM_CP",
                  "Latin America (R5)",      "Power",        "NGFS2021_GCAM_CP",
                  "Latin America (R5)",       "Coal",       "NGFS2021_GCAM_DN0",
                  "Latin America (R5)",    "Oil&Gas",       "NGFS2021_GCAM_DN0",
                  "Latin America (R5)",      "Power",       "NGFS2021_GCAM_DN0",
                  "Latin America (R5)",       "Coal",        "NGFS2021_GCAM_DT",
                  "Latin America (R5)",    "Oil&Gas",        "NGFS2021_GCAM_DT",
                  "Latin America (R5)",      "Power",        "NGFS2021_GCAM_DT",
                  "Latin America (R5)",       "Coal",       "NGFS2021_GCAM_NDC",
                  "Latin America (R5)",    "Oil&Gas",       "NGFS2021_GCAM_NDC",
                  "Latin America (R5)",      "Power",       "NGFS2021_GCAM_NDC",
                  "Latin America (R5)",       "Coal",    "NGFS2021_GCAM_NZ2050",
                  "Latin America (R5)",    "Oil&Gas",    "NGFS2021_GCAM_NZ2050",
                  "Latin America (R5)",      "Power",    "NGFS2021_GCAM_NZ2050",
                  "Latin America (R5)",       "Coal",   "NGFS2021_MESSAGE_B2DS",
                  "Latin America (R5)",    "Oil&Gas",   "NGFS2021_MESSAGE_B2DS",
                  "Latin America (R5)",      "Power",   "NGFS2021_MESSAGE_B2DS",
                  "Latin America (R5)",       "Coal",     "NGFS2021_MESSAGE_CP",
                  "Latin America (R5)",    "Oil&Gas",     "NGFS2021_MESSAGE_CP",
                  "Latin America (R5)",      "Power",     "NGFS2021_MESSAGE_CP",
                  "Latin America (R5)",       "Coal",    "NGFS2021_MESSAGE_DN0",
                  "Latin America (R5)",    "Oil&Gas",    "NGFS2021_MESSAGE_DN0",
                  "Latin America (R5)",      "Power",    "NGFS2021_MESSAGE_DN0",
                  "Latin America (R5)",       "Coal",     "NGFS2021_MESSAGE_DT",
                  "Latin America (R5)",    "Oil&Gas",     "NGFS2021_MESSAGE_DT",
                  "Latin America (R5)",      "Power",     "NGFS2021_MESSAGE_DT",
                  "Latin America (R5)",       "Coal",    "NGFS2021_MESSAGE_NDC",
                  "Latin America (R5)",    "Oil&Gas",    "NGFS2021_MESSAGE_NDC",
                  "Latin America (R5)",      "Power",    "NGFS2021_MESSAGE_NDC",
                  "Latin America (R5)",       "Coal", "NGFS2021_MESSAGE_NZ2050",
                  "Latin America (R5)",    "Oil&Gas", "NGFS2021_MESSAGE_NZ2050",
                  "Latin America (R5)",      "Power", "NGFS2021_MESSAGE_NZ2050",
                  "Latin America (R5)",       "Coal",    "NGFS2021_REMIND_B2DS",
                  "Latin America (R5)",    "Oil&Gas",    "NGFS2021_REMIND_B2DS",
                  "Latin America (R5)",      "Power",    "NGFS2021_REMIND_B2DS",
                  "Latin America (R5)",       "Coal",      "NGFS2021_REMIND_CP",
                  "Latin America (R5)",    "Oil&Gas",      "NGFS2021_REMIND_CP",
                  "Latin America (R5)",      "Power",      "NGFS2021_REMIND_CP",
                  "Latin America (R5)",       "Coal",     "NGFS2021_REMIND_DN0",
                  "Latin America (R5)",    "Oil&Gas",     "NGFS2021_REMIND_DN0",
                  "Latin America (R5)",      "Power",     "NGFS2021_REMIND_DN0",
                  "Latin America (R5)",       "Coal",      "NGFS2021_REMIND_DT",
                  "Latin America (R5)",    "Oil&Gas",      "NGFS2021_REMIND_DT",
                  "Latin America (R5)",      "Power",      "NGFS2021_REMIND_DT",
                  "Latin America (R5)",       "Coal",     "NGFS2021_REMIND_NDC",
                  "Latin America (R5)",    "Oil&Gas",     "NGFS2021_REMIND_NDC",
                  "Latin America (R5)",      "Power",     "NGFS2021_REMIND_NDC",
                  "Latin America (R5)",       "Coal",  "NGFS2021_REMIND_NZ2050",
                  "Latin America (R5)",    "Oil&Gas",  "NGFS2021_REMIND_NZ2050",
                  "Latin America (R5)",      "Power",  "NGFS2021_REMIND_NZ2050",
                        "LatinAmerica",       "Coal",             "WEO2021_APS",
                        "LatinAmerica",    "Oil&Gas",             "WEO2021_APS",
                        "LatinAmerica",       "Coal",             "WEO2021_SDS",
                        "LatinAmerica",    "Oil&Gas",             "WEO2021_SDS",
                        "LatinAmerica",       "Coal",           "WEO2021_STEPS",
                        "LatinAmerica",    "Oil&Gas",           "WEO2021_STEPS",
           "Middle East & Africa (R5)",       "Coal",      "NGFS2021_GCAM_B2DS",
           "Middle East & Africa (R5)",    "Oil&Gas",      "NGFS2021_GCAM_B2DS",
           "Middle East & Africa (R5)",      "Power",      "NGFS2021_GCAM_B2DS",
           "Middle East & Africa (R5)",       "Coal",        "NGFS2021_GCAM_CP",
           "Middle East & Africa (R5)",    "Oil&Gas",        "NGFS2021_GCAM_CP",
           "Middle East & Africa (R5)",      "Power",        "NGFS2021_GCAM_CP",
           "Middle East & Africa (R5)",       "Coal",       "NGFS2021_GCAM_DN0",
           "Middle East & Africa (R5)",    "Oil&Gas",       "NGFS2021_GCAM_DN0",
           "Middle East & Africa (R5)",      "Power",       "NGFS2021_GCAM_DN0",
           "Middle East & Africa (R5)",       "Coal",        "NGFS2021_GCAM_DT",
           "Middle East & Africa (R5)",    "Oil&Gas",        "NGFS2021_GCAM_DT",
           "Middle East & Africa (R5)",      "Power",        "NGFS2021_GCAM_DT",
           "Middle East & Africa (R5)",       "Coal",       "NGFS2021_GCAM_NDC",
           "Middle East & Africa (R5)",    "Oil&Gas",       "NGFS2021_GCAM_NDC",
           "Middle East & Africa (R5)",      "Power",       "NGFS2021_GCAM_NDC",
           "Middle East & Africa (R5)",       "Coal",    "NGFS2021_GCAM_NZ2050",
           "Middle East & Africa (R5)",    "Oil&Gas",    "NGFS2021_GCAM_NZ2050",
           "Middle East & Africa (R5)",      "Power",    "NGFS2021_GCAM_NZ2050",
           "Middle East & Africa (R5)",       "Coal",   "NGFS2021_MESSAGE_B2DS",
           "Middle East & Africa (R5)",    "Oil&Gas",   "NGFS2021_MESSAGE_B2DS",
           "Middle East & Africa (R5)",      "Power",   "NGFS2021_MESSAGE_B2DS",
           "Middle East & Africa (R5)",       "Coal",     "NGFS2021_MESSAGE_CP",
           "Middle East & Africa (R5)",    "Oil&Gas",     "NGFS2021_MESSAGE_CP",
           "Middle East & Africa (R5)",      "Power",     "NGFS2021_MESSAGE_CP",
           "Middle East & Africa (R5)",       "Coal",    "NGFS2021_MESSAGE_DN0",
           "Middle East & Africa (R5)",    "Oil&Gas",    "NGFS2021_MESSAGE_DN0",
           "Middle East & Africa (R5)",      "Power",    "NGFS2021_MESSAGE_DN0",
           "Middle East & Africa (R5)",       "Coal",     "NGFS2021_MESSAGE_DT",
           "Middle East & Africa (R5)",    "Oil&Gas",     "NGFS2021_MESSAGE_DT",
           "Middle East & Africa (R5)",      "Power",     "NGFS2021_MESSAGE_DT",
           "Middle East & Africa (R5)",       "Coal",    "NGFS2021_MESSAGE_NDC",
           "Middle East & Africa (R5)",    "Oil&Gas",    "NGFS2021_MESSAGE_NDC",
           "Middle East & Africa (R5)",      "Power",    "NGFS2021_MESSAGE_NDC",
           "Middle East & Africa (R5)",       "Coal", "NGFS2021_MESSAGE_NZ2050",
           "Middle East & Africa (R5)",    "Oil&Gas", "NGFS2021_MESSAGE_NZ2050",
           "Middle East & Africa (R5)",      "Power", "NGFS2021_MESSAGE_NZ2050",
           "Middle East & Africa (R5)",       "Coal",    "NGFS2021_REMIND_B2DS",
           "Middle East & Africa (R5)",    "Oil&Gas",    "NGFS2021_REMIND_B2DS",
           "Middle East & Africa (R5)",      "Power",    "NGFS2021_REMIND_B2DS",
           "Middle East & Africa (R5)",       "Coal",      "NGFS2021_REMIND_CP",
           "Middle East & Africa (R5)",    "Oil&Gas",      "NGFS2021_REMIND_CP",
           "Middle East & Africa (R5)",      "Power",      "NGFS2021_REMIND_CP",
           "Middle East & Africa (R5)",       "Coal",     "NGFS2021_REMIND_DN0",
           "Middle East & Africa (R5)",    "Oil&Gas",     "NGFS2021_REMIND_DN0",
           "Middle East & Africa (R5)",      "Power",     "NGFS2021_REMIND_DN0",
           "Middle East & Africa (R5)",       "Coal",      "NGFS2021_REMIND_DT",
           "Middle East & Africa (R5)",    "Oil&Gas",      "NGFS2021_REMIND_DT",
           "Middle East & Africa (R5)",      "Power",      "NGFS2021_REMIND_DT",
           "Middle East & Africa (R5)",       "Coal",     "NGFS2021_REMIND_NDC",
           "Middle East & Africa (R5)",    "Oil&Gas",     "NGFS2021_REMIND_NDC",
           "Middle East & Africa (R5)",      "Power",     "NGFS2021_REMIND_NDC",
           "Middle East & Africa (R5)",       "Coal",  "NGFS2021_REMIND_NZ2050",
           "Middle East & Africa (R5)",    "Oil&Gas",  "NGFS2021_REMIND_NZ2050",
           "Middle East & Africa (R5)",      "Power",  "NGFS2021_REMIND_NZ2050",
                          "MiddleEast",       "Coal",             "WEO2021_APS",
                          "MiddleEast",    "Oil&Gas",             "WEO2021_APS",
                          "MiddleEast",      "Power",             "WEO2021_APS",
                          "MiddleEast",       "Coal",             "WEO2021_SDS",
                          "MiddleEast",    "Oil&Gas",             "WEO2021_SDS",
                          "MiddleEast",      "Power",             "WEO2021_SDS",
                          "MiddleEast",       "Coal",           "WEO2021_STEPS",
                          "MiddleEast",    "Oil&Gas",           "WEO2021_STEPS",
                          "MiddleEast",      "Power",           "WEO2021_STEPS",
                             "NonOECD",       "Coal",             "WEO2021_APS",
                             "NonOECD",    "Oil&Gas",             "WEO2021_APS",
                             "NonOECD",      "Power",             "WEO2021_APS",
                             "NonOECD",       "Coal",             "WEO2021_SDS",
                             "NonOECD",    "Oil&Gas",             "WEO2021_SDS",
                             "NonOECD",      "Power",             "WEO2021_SDS",
                             "NonOECD",       "Coal",           "WEO2021_STEPS",
                             "NonOECD",    "Oil&Gas",           "WEO2021_STEPS",
                             "NonOECD",      "Power",           "WEO2021_STEPS",
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
                      "OECD & EU (R5)",       "Coal",      "NGFS2021_GCAM_B2DS",
                      "OECD & EU (R5)",    "Oil&Gas",      "NGFS2021_GCAM_B2DS",
                      "OECD & EU (R5)",      "Power",      "NGFS2021_GCAM_B2DS",
                      "OECD & EU (R5)",       "Coal",        "NGFS2021_GCAM_CP",
                      "OECD & EU (R5)",    "Oil&Gas",        "NGFS2021_GCAM_CP",
                      "OECD & EU (R5)",      "Power",        "NGFS2021_GCAM_CP",
                      "OECD & EU (R5)",       "Coal",       "NGFS2021_GCAM_DN0",
                      "OECD & EU (R5)",    "Oil&Gas",       "NGFS2021_GCAM_DN0",
                      "OECD & EU (R5)",      "Power",       "NGFS2021_GCAM_DN0",
                      "OECD & EU (R5)",       "Coal",        "NGFS2021_GCAM_DT",
                      "OECD & EU (R5)",    "Oil&Gas",        "NGFS2021_GCAM_DT",
                      "OECD & EU (R5)",      "Power",        "NGFS2021_GCAM_DT",
                      "OECD & EU (R5)",       "Coal",       "NGFS2021_GCAM_NDC",
                      "OECD & EU (R5)",    "Oil&Gas",       "NGFS2021_GCAM_NDC",
                      "OECD & EU (R5)",      "Power",       "NGFS2021_GCAM_NDC",
                      "OECD & EU (R5)",       "Coal",    "NGFS2021_GCAM_NZ2050",
                      "OECD & EU (R5)",    "Oil&Gas",    "NGFS2021_GCAM_NZ2050",
                      "OECD & EU (R5)",      "Power",    "NGFS2021_GCAM_NZ2050",
                      "OECD & EU (R5)",       "Coal",   "NGFS2021_MESSAGE_B2DS",
                      "OECD & EU (R5)",    "Oil&Gas",   "NGFS2021_MESSAGE_B2DS",
                      "OECD & EU (R5)",      "Power",   "NGFS2021_MESSAGE_B2DS",
                      "OECD & EU (R5)",       "Coal",     "NGFS2021_MESSAGE_CP",
                      "OECD & EU (R5)",    "Oil&Gas",     "NGFS2021_MESSAGE_CP",
                      "OECD & EU (R5)",      "Power",     "NGFS2021_MESSAGE_CP",
                      "OECD & EU (R5)",       "Coal",    "NGFS2021_MESSAGE_DN0",
                      "OECD & EU (R5)",    "Oil&Gas",    "NGFS2021_MESSAGE_DN0",
                      "OECD & EU (R5)",      "Power",    "NGFS2021_MESSAGE_DN0",
                      "OECD & EU (R5)",       "Coal",     "NGFS2021_MESSAGE_DT",
                      "OECD & EU (R5)",    "Oil&Gas",     "NGFS2021_MESSAGE_DT",
                      "OECD & EU (R5)",      "Power",     "NGFS2021_MESSAGE_DT",
                      "OECD & EU (R5)",       "Coal",    "NGFS2021_MESSAGE_NDC",
                      "OECD & EU (R5)",    "Oil&Gas",    "NGFS2021_MESSAGE_NDC",
                      "OECD & EU (R5)",      "Power",    "NGFS2021_MESSAGE_NDC",
                      "OECD & EU (R5)",       "Coal", "NGFS2021_MESSAGE_NZ2050",
                      "OECD & EU (R5)",    "Oil&Gas", "NGFS2021_MESSAGE_NZ2050",
                      "OECD & EU (R5)",      "Power", "NGFS2021_MESSAGE_NZ2050",
                      "OECD & EU (R5)",       "Coal",    "NGFS2021_REMIND_B2DS",
                      "OECD & EU (R5)",    "Oil&Gas",    "NGFS2021_REMIND_B2DS",
                      "OECD & EU (R5)",      "Power",    "NGFS2021_REMIND_B2DS",
                      "OECD & EU (R5)",       "Coal",      "NGFS2021_REMIND_CP",
                      "OECD & EU (R5)",    "Oil&Gas",      "NGFS2021_REMIND_CP",
                      "OECD & EU (R5)",      "Power",      "NGFS2021_REMIND_CP",
                      "OECD & EU (R5)",       "Coal",     "NGFS2021_REMIND_DN0",
                      "OECD & EU (R5)",    "Oil&Gas",     "NGFS2021_REMIND_DN0",
                      "OECD & EU (R5)",      "Power",     "NGFS2021_REMIND_DN0",
                      "OECD & EU (R5)",       "Coal",      "NGFS2021_REMIND_DT",
                      "OECD & EU (R5)",    "Oil&Gas",      "NGFS2021_REMIND_DT",
                      "OECD & EU (R5)",      "Power",      "NGFS2021_REMIND_DT",
                      "OECD & EU (R5)",       "Coal",     "NGFS2021_REMIND_NDC",
                      "OECD & EU (R5)",    "Oil&Gas",     "NGFS2021_REMIND_NDC",
                      "OECD & EU (R5)",      "Power",     "NGFS2021_REMIND_NDC",
                      "OECD & EU (R5)",       "Coal",  "NGFS2021_REMIND_NZ2050",
                      "OECD & EU (R5)",    "Oil&Gas",  "NGFS2021_REMIND_NZ2050",
                      "OECD & EU (R5)",      "Power",  "NGFS2021_REMIND_NZ2050",
            "Reforming Economies (R5)",       "Coal",      "NGFS2021_GCAM_B2DS",
            "Reforming Economies (R5)",    "Oil&Gas",      "NGFS2021_GCAM_B2DS",
            "Reforming Economies (R5)",      "Power",      "NGFS2021_GCAM_B2DS",
            "Reforming Economies (R5)",       "Coal",        "NGFS2021_GCAM_CP",
            "Reforming Economies (R5)",    "Oil&Gas",        "NGFS2021_GCAM_CP",
            "Reforming Economies (R5)",      "Power",        "NGFS2021_GCAM_CP",
            "Reforming Economies (R5)",       "Coal",       "NGFS2021_GCAM_DN0",
            "Reforming Economies (R5)",    "Oil&Gas",       "NGFS2021_GCAM_DN0",
            "Reforming Economies (R5)",      "Power",       "NGFS2021_GCAM_DN0",
            "Reforming Economies (R5)",       "Coal",        "NGFS2021_GCAM_DT",
            "Reforming Economies (R5)",    "Oil&Gas",        "NGFS2021_GCAM_DT",
            "Reforming Economies (R5)",      "Power",        "NGFS2021_GCAM_DT",
            "Reforming Economies (R5)",       "Coal",       "NGFS2021_GCAM_NDC",
            "Reforming Economies (R5)",    "Oil&Gas",       "NGFS2021_GCAM_NDC",
            "Reforming Economies (R5)",      "Power",       "NGFS2021_GCAM_NDC",
            "Reforming Economies (R5)",       "Coal",    "NGFS2021_GCAM_NZ2050",
            "Reforming Economies (R5)",    "Oil&Gas",    "NGFS2021_GCAM_NZ2050",
            "Reforming Economies (R5)",      "Power",    "NGFS2021_GCAM_NZ2050",
            "Reforming Economies (R5)",       "Coal",   "NGFS2021_MESSAGE_B2DS",
            "Reforming Economies (R5)",    "Oil&Gas",   "NGFS2021_MESSAGE_B2DS",
            "Reforming Economies (R5)",      "Power",   "NGFS2021_MESSAGE_B2DS",
            "Reforming Economies (R5)",       "Coal",     "NGFS2021_MESSAGE_CP",
            "Reforming Economies (R5)",    "Oil&Gas",     "NGFS2021_MESSAGE_CP",
            "Reforming Economies (R5)",      "Power",     "NGFS2021_MESSAGE_CP",
            "Reforming Economies (R5)",       "Coal",    "NGFS2021_MESSAGE_DN0",
            "Reforming Economies (R5)",    "Oil&Gas",    "NGFS2021_MESSAGE_DN0",
            "Reforming Economies (R5)",      "Power",    "NGFS2021_MESSAGE_DN0",
            "Reforming Economies (R5)",       "Coal",     "NGFS2021_MESSAGE_DT",
            "Reforming Economies (R5)",    "Oil&Gas",     "NGFS2021_MESSAGE_DT",
            "Reforming Economies (R5)",      "Power",     "NGFS2021_MESSAGE_DT",
            "Reforming Economies (R5)",       "Coal",    "NGFS2021_MESSAGE_NDC",
            "Reforming Economies (R5)",    "Oil&Gas",    "NGFS2021_MESSAGE_NDC",
            "Reforming Economies (R5)",      "Power",    "NGFS2021_MESSAGE_NDC",
            "Reforming Economies (R5)",       "Coal", "NGFS2021_MESSAGE_NZ2050",
            "Reforming Economies (R5)",    "Oil&Gas", "NGFS2021_MESSAGE_NZ2050",
            "Reforming Economies (R5)",      "Power", "NGFS2021_MESSAGE_NZ2050",
            "Reforming Economies (R5)",       "Coal",    "NGFS2021_REMIND_B2DS",
            "Reforming Economies (R5)",    "Oil&Gas",    "NGFS2021_REMIND_B2DS",
            "Reforming Economies (R5)",      "Power",    "NGFS2021_REMIND_B2DS",
            "Reforming Economies (R5)",       "Coal",      "NGFS2021_REMIND_CP",
            "Reforming Economies (R5)",    "Oil&Gas",      "NGFS2021_REMIND_CP",
            "Reforming Economies (R5)",      "Power",      "NGFS2021_REMIND_CP",
            "Reforming Economies (R5)",       "Coal",     "NGFS2021_REMIND_DN0",
            "Reforming Economies (R5)",    "Oil&Gas",     "NGFS2021_REMIND_DN0",
            "Reforming Economies (R5)",      "Power",     "NGFS2021_REMIND_DN0",
            "Reforming Economies (R5)",       "Coal",      "NGFS2021_REMIND_DT",
            "Reforming Economies (R5)",    "Oil&Gas",      "NGFS2021_REMIND_DT",
            "Reforming Economies (R5)",      "Power",      "NGFS2021_REMIND_DT",
            "Reforming Economies (R5)",       "Coal",     "NGFS2021_REMIND_NDC",
            "Reforming Economies (R5)",    "Oil&Gas",     "NGFS2021_REMIND_NDC",
            "Reforming Economies (R5)",      "Power",     "NGFS2021_REMIND_NDC",
            "Reforming Economies (R5)",       "Coal",  "NGFS2021_REMIND_NZ2050",
            "Reforming Economies (R5)",    "Oil&Gas",  "NGFS2021_REMIND_NZ2050",
            "Reforming Economies (R5)",      "Power",  "NGFS2021_REMIND_NZ2050",
                              "Russia",      "Power",             "WEO2021_APS",
                              "Russia",      "Power",             "WEO2021_SDS",
                              "Russia",      "Power",           "WEO2021_STEPS",
                                  "US",      "Power",             "WEO2021_APS",
                                  "US",      "Power",             "WEO2021_SDS",
                                  "US",      "Power",           "WEO2021_STEPS"
  )

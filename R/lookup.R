# technology and sector mapping between P4I and P4B
# HDV and shipping not consistently defined across both versions at this time
# styler: off
p4i_p4b_sector_technology_lookup <- tibble::tribble(
  ~sector_p4b,   ~technology_p4b,             ~sector_p4i,    ~technology_p4i,
  "automotive",  "electric",                 "Automotive",   "Electric",
  "automotive",  "hybrid",                   "Automotive",   "Hybrid",
  "automotive",  "ice",                      "Automotive",   "ICE",
  "coal",        "coal",                     "Coal",         "Coal",
  "oil and gas", "gas",                      "Oil&Gas",      "Gas",
  "oil and gas", "oil",                      "Oil&Gas",      "Oil",
  "power",       "coalcap",                  "Power",        "CoalCap",
  "power",       "gascap",                   "Power",        "GasCap",
  "power",       "hydrocap",                 "Power",        "HydroCap",
  "power",       "nuclearcap",               "Power",        "NuclearCap",
  "power",       "oilcap",                   "Power",        "OilCap",
  "power",       "renewablescap",            "Power",        "RenewablesCap",
  "aviation",    "freight",                  "Aviation",     "Freight",
  "aviation",    "passenger",                "Aviation",     "Passenger",
  "cement",      "grinding",                 "Cement",       "Grinding",
  "cement",      "integrated facility",      "Cement",       "Integrated facility",
  "steel",       "ac-electric arc furnace",  "Steel",        "Ac-Electric Arc Furnace",
  "steel",       "bof shop",                 "Steel",        "Bof Shop",
  "steel",       "dc-electric arc furnace",  "Steel",        "Dc-Electric Arc Furnace",
  "steel",       "open hearth meltshop",     "Steel",        "Open Hearth Meltshop"
)

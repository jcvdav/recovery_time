#   Clean conapesca data and get population growth rates    #

# Load libraries
library(here)
library(furrr)
library(tidyverse)

file <- "/Users/juancarlosvillasenorderbez/GitHub/data_mex_fisheries/data/mex_landings/clean/mex_conapesca_avisos_2000_2019.rds"

# Define list of specie sof interest
spp_interest <- c("Panulirus interruptus",
                  "Haliotis spp",
                  "Strongylocentrotus spp",
                  "Parastichopus parvimensis",
                  "Magastraea spp",
                  "Paralabrax nebulifer",
                  "Paralabrax maculatofasciatus",
                  "Semicossyphus pulcher",
                  "Atractoscion nobilis",
                  "Flatfish",
                  "Stereolepis gigas")

# Define groups of species
## Abalone
haliotis <- c("Haliotis assimilis",
              "Haliotis corrugata",
              "Haliotis cracherodii",
              "Haliotis fulgens",
              "Haliotis rufescens",
              "Haliotis sorenseni",
              "Haliots sorenseni")

## Sea urchins
strongilocentrotus <- c("Strongylocentrotus purpuratus",
                        "Strongylocentrotus franciscanus",
                        "Mesocentrotus franciscanus")

# Define offices from which we want data (Pacific side only)
# office <- c("Ensenada",
#             "El rosario",
#             "Guerrero negro",
#             "Cd. constitucion",
#             "Bahia asuncion",
#             "Bahia de los angeles",
#             "Bahia tortugas",
#             "San carlos",
#             "Punta abreojos",
#             "Isla cedros",
#             "Pto. adolfo lopez mateos",
#             "Tijuana", 
#             "San quintin",
#             "Villa de jesus maria")

office <- c("BAHIA ASUNCION", 
            "BAHIA TORTUGAS",
            # "CD. CONSTITUCION", # Too far south
            "EL ROSARIO",
            "ENSENADA",
            "GUERRERO NEGRO",
            "ISLA CEDROS",
            # "PTO. ADOLFO LOPEZ MATEOS",  # Too far south
            "PUNTA ABREOJOS",
            "SAN CARLOS",
            # "SAN JUANICO",  # Too far south
            "SAN QUINTIN",
            "VILLA DE JESUS MARIA",
            "TIJUANA")

abalone_codes <- c("0011023H",
                   "0031427H",
                   "0021428H",
                   "0021022H",
                   "0031021H",
                   "0051029H",
                   "0011429H",
                   "0061424H",
                   "0071423H",
                   "0071027H",
                   "0041426H",
                   "0041020H",
                   "0081026H",
                   "0051425H")

lobster_codes <- c("3751427H",
                   "3711421H",
                   "3751427D",
                   "3710720H",
                   "3750726H",
                   "3711728H")

cucumber_codes <- c("3001427H",
                    "3001428H",
                    "3001429H")

urchin_codes <- c("3081429H",
                  "3091425H",
                  "3101422H",
                  "3091727H",
                  "3101725H",
                  "3081726H")

# FINFISH
vieja_codes <- c("3671427H",
                 "3671625H",
                 "3671724H")

sandbass_codes <- c("7101629H",
                    "7101421H")

flatfish_codes <- c("3981628H",
                    "3981420H",
                    "3980927H",
                    "3981727H")

species_codes <- c(abalone_codes,
                   lobster_codes,
                   urchin_codes,
                   cucumber_codes,
                   vieja_codes,
                   sandbass_codes,
                   flatfish_codes)

# Read conapesca data
cona_clean <- readRDS(file) %>% 
  filter(state %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR")) %>% 
  filter(office_name %in% office) %>% 
  filter(species_key %in% species_codes) %>% 
  filter(year_cut <= 2013) %>% 
  mutate(taxa = case_when(
    species_key %in% abalone_codes ~ "Abalones",
    species_key %in% lobster_codes ~ "Lobster",
    species_key %in% urchin_codes ~ "Urchins",
    species_key %in% cucumber_codes ~ "Cucumbers",
    species_key %in% vieja_codes ~ "Sheephead",
    species_key %in% sandbass_codes ~ "Sandbass"#,
    #species_key %in% flatfish_codes ~ "Flatfish"
    )
    ) %>% 
  group_by(taxa, year_cut) %>% 
  summarize(landed_weight = sum(landed_weight, na.rm = T))

# Write the data
write.csv(x = cona_clean,
          file = here("data", "conapesca_baja_ts_until_2013.csv"),
          row.names = F)
















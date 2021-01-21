#   Clean conapesca data and get population growth rates    #

# Load libraries
library(here)
library(furrr)
library(tidyverse)

# Define list of specie sof interest
spp_interest <- c("Panulirus interruptus", "Haliotis spp", "Strongylocentrotus spp", "Parastichopus parvimensis", "Magastraea spp", "Paralabrax nebulifer", "Paralabrax maculatofasciatus", "Semmycosyphus pulcher", "Atractoscion nobilis", "Flatfish", "Stereolepis gigas", "Caulolatilus princeps")

# Define groups of species
## Abalone
haliotis <- c("Haliotis assimilis", "Haliotis corrugata", "Haliotis cracherodii", "Haliotis fulgens", "Haliotis rufescens", "Haliotis sorenseni", "Haliots sorenseni")

## Sea urchins
strongilocentrotus <- c("Strongylocentrotus purpuratus", "Strongylocentrotus franciscanus")

# Define offices from which we want data (Pacific side only)
office <- c("Ensenada", "El rosario", "Guerrero negro", "Cd. constitucion", "Bahia asuncion", "Bahia de los angeles", "Bahia tortugas", "San carlos", "Punta abreojos", "Isla cedros", "Pto. adolgo lopez mateos", "Tijuana", "San quintin", "Villa de jesus maria")

# Read conapesca data
cona <- readRDS(here("raw_data", "conapesca.rds")) %>% 
  mutate_at(.vars = vars(Estado, NombreCientifico), as.character) %>% 
  filter(Estado %in% c("Baja california", "Baja california sur")) %>% 
  filter(Oficina %in% office) %>% 
  mutate(NombreCientifico = case_when(NombreCientifico %in% haliotis ~ "Haliotis spp",
                                      NombreCientifico == "Paralichthyidae spp/pleuronectidae spp" ~ "Flatfish",
                                      NombreCientifico == "Pescara stereolepis gigas"~ "Stereolepis gigas",
                                      NombreCientifico %in% strongilocentrotus ~ "Strongylocentrotus spp",
                                      T ~ NombreCientifico)) %>% 
  filter(NombreCientifico %in% spp_interest) %>% 
  group_by(NombreCientifico, Ano) %>% 
  summarize(PesoVivo = sum(PesoVivo, na.rm = T),
            Valor = sum(Valor, na.rm = T))

# Write the data
write.csv(x = cona,
          file = here("data", "conapesca_baja_ts.csv"),
          row.names = F)
















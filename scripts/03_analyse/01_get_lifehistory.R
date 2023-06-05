#   get life history params   #

# Load libraries
library(startR)
library(here)
library(datalimited2)
library(furrr)
library(tidyverse)

# Load database
cona <- read.csv(#here("data", "conapesca_baja_ts.csv"),
  here("data", "conapesca_baja_ts_until_2013.csv"),
  stringsAsFactors = F) %>% 
  group_by(taxa) %>% 
  mutate(n = n()) %>%  
  ungroup() %>% 
  filter(n > 5)

# Create a function to wrap around mcsy2
# for each species to run it in parallel
my_mcsy2 <- function(data) {
  spp <- data$spp[1]
  if ((dim(data)[1] < 5)){
    results <- NA
  }
  
  else {
    year <- data$year_cut
    catch <- data$landed_weight
    results <- cmsy2(year = year, catch = catch, r.low = 0.015, r.hi = 1.5, verbose = F)
    
    saveRDS(results, here("results", "cmsy2", paste0(spp, ".rds")))
    results <- tibble(r_mean = mean(results$r_viable),
                      r_median = median(results$r_viable),
                      k_mean = mean(results$k_viable),
                      k_median = median(results$k_viable),
                      fmsy_mean = results$ref_pts$est[results$ref_pts$param == "fmsy"],
                      fmsy_lo = results$ref_pts$lo[results$ref_pts$param == "fmsy"])
  }
  
  return(results)
}

plan(multisession, workers = parallel::detectCores() - 2)
parameters <- cona %>% 
  mutate(spp = taxa) %>% 
  group_by(taxa) %>% 
  nest() %>% 
  mutate(cmsy_output = future_map(data, my_mcsy2, .options = furrr_options(seed = 123))) %>% 
  select(-data, species = taxa) %>% 
  unnest()
plan(sequential)


write.csv(x = parameters,
          file = here("data", "updated_parameters_2013.csv"),
          row.names = F)


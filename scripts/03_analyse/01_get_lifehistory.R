#   get life history params   #

# Load libraries
library(startR)
library(here)
library(datalimited2)
library(readxl)
library(furrr)
library(tidyverse)

# Load database
cona <- read.csv(here("data", "conapesca_baja_ts.csv"),
                 stringsAsFactors = F) %>% 
  group_by(NombreCientifico) %>% 
  mutate(n = n()) %>%  
  ungroup() %>% 
  filter(n > 5)

dat <- tibble(filename = list.files(here("data", "poncho_data"), full.names = T)) %>% 
  mutate(spp = basename(filename),
         data = map(filename, read_excel)) %>% 
  unnest(data) %>% 
  janitor::clean_names() %>% 
  rename(Ano = ano) %>% 
  mutate(Ano = coalesce(Ano, ano_corte)) %>% 
  group_by(spp, Ano) %>% 
  summarize(PesoVivo = sum(peso_vivo_kilogramos, na.rm = T))

# Create a function to wrap around mcsy2
# for each species to run it in parallel
my_mcsy2 <- function(data) {
  spp <- data$spp[1]
  if ((dim(data)[1] < 5)){
    results <- NA
  }
  
  else {
    year <- data$Ano
    catch <- data$PesoVivo
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

parameters <- dat %>% 
  group_by(spp) %>% 
  nest() %>% 
  mutate(cmsy_output = map(data, my_mcsy2)) %>% 
  select(-data, species = spp) %>% 
  unnest()

# plan(multisession, workers = parallel::detectCores() - 2)
parameters <- cona %>% 
  mutate(spp = NombreCientifico) %>% 
  group_by(NombreCientifico) %>% 
  nest() %>% 
  mutate(cmsy_output = map(data, my_mcsy2)) %>% 
  select(-data, species = NombreCientifico) %>% 
  unnest(cmsy_output)
# plan(sequential)

write.csv(x = parameters,
          file = here("data", "parameters.csv"),
          row.names = F)

(catches_ts <- ggplot(data = cona,
       mapping = aes(x = Ano, y = PesoVivo / 1e3)) +
  geom_line() +
  geom_point(fill = "steelblue", color = "black", size = 2, shape = 21) +
  facet_wrap(~NombreCientifico, scales = "free_y") +
  ggtheme_plot() +
  labs(x = "Year", y = "Landings\n(Metric Tones)"))

ggsave(plot = catches_ts,
       filename = here("results", "img", "catches_ts.tiff"),
       width = 6,
       height = 4)

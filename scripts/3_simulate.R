# Simulate a spatial closure #

# Load packages
library(startR)
library(here)
library(tidyverse)

# Load data
parameters <- read.csv(here("data", "parameters.csv"),
                       stringsAsFactors = F) %>% 
  mutate(species = ifelse(species == "Strongylocentrotus spp", "Sea urchins", species))

# Define functions
## logistic growth function with harvesting and
## a spatially implicit reserve
grow <- function(x, r, K, fmsy, R) {
  res <- x + (r * x * (1 - (x / K))) - ((1 - R) * x * fmsy)
  return(res)
}


## Iterate through time for each species
move <- function(nyears, r, K, fmsy, R) {
  x0 <- 0.2 * K
  X <- numeric(nyears)
  X[1] <- x0
  
    for(i in 2:nyears) {
    X[i] <- grow(x = X[i - 1], r = r, K = K, fmsy = fmsy, R = R)
  }
  
  results <- tibble(X, time = 1:nyears)
  
  return(results)
}

nyears <- 30

results <- parameters %>% 
  mutate(R = 0.1) %>% 
  rbind(parameters %>% mutate(R = 0.3)) %>% 
  rbind(parameters %>% mutate(R = 0.5)) %>% 
  rbind(parameters %>% mutate(R = 1)) %>% 
  mutate(simulation = purrr::pmap(.l = list(nyears = nyears, r = r_mean, K = k_mean, fmsy = fmsy_lo, R = R), .f = move)) %>% 
  unnest() %>% 
  mutate(Xnorm = X / k_mean) %>% 
  mutate(R = paste0("Reserve = ", R * 100, "%"),
         R = fct_relevel(R, "Reserve = 100%", after = Inf))

(recovery_plot <- results %>% 
    ggplot(aes(x = time, y = Xnorm, color = species)) +
    geom_line(size = 1) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, 30, by = 5)) +
    labs(x = "Time (years)", y = "Normalized biomass (B / K)") +
    scale_color_brewer(palette = "Set1") +
    theme(strip.background = element_blank(),
          legend.position = "bottom",
          text = element_text(size = 10)) +
    guides(color = guide_legend(title = "Species",
                                ncol = 2,
                                label.theme = element_text(face = "italic",
                                                           size = 8))) +
    facet_wrap(~R, ncol = 1))


ggsave(plot = recovery_plot,
       file = here("results", "img", "recovery_plot.png"),
       height = 7,
       width = 4.1)




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
  X <- Eq <- numeric(nyears)
  X[1] <- x0
  Eq[1] <- F
  
  for(i in 2:nyears) {
    X[i] <- grow(x = X[i - 1], r = r, K = K, fmsy = fmsy, R = R)
    Eq[i] <- X[i] >= (0.5 * K)
  }
  
  results <- tibble(X,
                    time = 1:nyears,
                    Eq)
  
  return(results)
}

nyears <- 50

results <- parameters %>% 
  mutate(R = 0.1) %>% 
  rbind(parameters %>% mutate(R = 0.3)) %>% 
  rbind(parameters %>% mutate(R = 1)) %>%
  mutate(simulation = purrr::pmap(.l = list(nyears = nyears,
                                            r = r_mean,
                                            K = k_mean,
                                            fmsy = fmsy_lo,
                                            R = R),
                                  .f = move)) %>% 
  unnest(cols = simulation) %>% 
  mutate(Xnorm = X / k_mean) %>% 
  mutate(R = paste0("Reserve = ", R * 100, "%"),
         R = fct_relevel(R, "Reserve = 100%", after = Inf))



(time_to_k <- results %>% 
    filter(Eq == 1) %>%
    group_by(species, R) %>% 
    mutate(min_t = min(time)) %>% 
    ungroup() %>% 
    filter(time == min_t) %>%
    select(time, species, R) %>% 
    complete(R, nesting(species), fill = list(time = 0)) %>%
    ggplot(aes(x = species, y = time, fill = R)) +
    geom_col(position = "dodge", color = "black") +
    scale_fill_brewer(palette = "Set1") +
    coord_flip() +
    ggtheme_plot() +
    labs(y = "Time to reach K/2",
         x = "Species") +
    theme(legend.justification = c(1, 1),
          legend.position = c(1, 1)))

ggsave(plot = time_to_k,
       file = here("results", "img", "time_to_k.png"),
       height = 5,
       width = 6)

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


rs <- seq(0.1, 1.2, length.out = 50)
Rs <- seq(0.1, 1, length.out = 50)

combinations <- expand.grid(r = rs, R = Rs) %>% 
  as_tibble() %>% 
  mutate(simulation = purrr::pmap(.l = list(nyears = nyears,
                                            r = r,
                                            K = 10000,
                                            fmsy = 0.1,
                                            R = R),
                                  .f = move)) %>% 
  unnest(cols = simulation) %>% 
  filter(Eq == 1) %>%
  group_by(r, R) %>% 
  mutate(min_t = min(time)) %>% 
  ungroup() %>% 
  filter(time == min_t) %>% 
  complete(r, nesting(R), fill = list(time = NA)) %>% 
  ggplot(aes(x = r, y = R, fill = time)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c() +
  ggtheme_plot() +
  labs(x = "Intrinsic growth rate (r)",
       y = "% as Reserve") +
  guides(fill = guide_colorbar(title = "Time to K/2"))

ggsave(plot = combinations,
       file = here("results", "img", "combinations.png"),
       height = 4,
       width = 4)


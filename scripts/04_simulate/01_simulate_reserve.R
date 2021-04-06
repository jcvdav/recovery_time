# Simulate a spatial closure #

# Load packages
library(startR)
library(here)
library(tidyverse)

## Reserve simulation parameters
nyears <- 50            # Number of years to simulate
nsim <- 1e4             # Number of repeated simulations
R_vec <- c(0.1, 0.3, 1) # Reserve-size scenarios

# Load data
# MHW effects
effects <- readRDS(here("data", "MHW_effects_on_inverts.rds"))

# Population parameters
parameters <- read.csv(here("data", "parameters.csv"),
                       stringsAsFactors = F) %>% 
  mutate(species = ifelse(species == "Strongylocentrotus spp", "Sea urchins", species)) %>% 
  left_join(effects, by = "species") %>% 
  replace_na(replace = list(MHW_effect = 0))

# Heatwave models
cc_scenarios <- readRDS(here("data", "MHW_models.rds")) %>% 
  filter(str_detect(dep_var, "Mean"))

### Simulate

simulations <- expand_grid(parameters, R = R_vec) %>% 
  mutate(SSP = "Deterministic") %>% 
  mutate(simulation = purrr::pmap(.l = list(nyears = nyears,
                                            r = r_mean,
                                            K = k_mean,
                                            fmsy = fmsy_lo,
                                            R = R,
                                            delta = 0,
                                            model = NA,
                                            nsim = 1),
                                  .f = sim_reserve)) %>% 
  select(species, SSP, simulation, contains("mean"), R) %>% 
  unnest(cols = simulation) %>% 
  mutate(Xnorm = X / k_mean)

cc_simulations <- expand_grid(parameters, cc_scenarios, R = R_vec) %>% 
  filter(MHW_effect > 0) %>% 
  mutate(simulation = purrr::pmap(.l = list(nyears = nyears,
                                            r = r_mean,
                                            K = k_mean,
                                            fmsy = fmsy_lo,
                                            R = R,
                                            delta = MHW_effect,
                                            model = model,
                                            nsim = nsim),
                                  .f = sim_reserve)) %>% 
  select(species, SSP, simulation, contains("mean"), R) %>% 
  unnest(cols = simulation) %>% 
  mutate(Xnorm = X / k_mean)

# Combine deterministic and CC results
results <- rbind(simulations,
                 cc_simulations)

summarize_results <- results %>% 
  group_by(species, time, SSP, R) %>% 
  summarize(x_mean = mean(Xnorm, na.rm = T),
            x_2.5 = quantile(Xnorm, 0.05, na.rm = T),
            x_97.5 = quantile(Xnorm, 0.95, na.rm = T)) %>% 
  mutate(R = paste0("Reserve = ", R * 100, "%"),
         R = fct_relevel(R, "Reserve = 100%", after = Inf))

(time_to_k <- simulations %>% 
    mutate(R = paste0("Reserve = ", R * 100, "%"),
           R = fct_relevel(R, "Reserve = 100%", after = Inf)) %>% 
    group_by(species, R) %>%
    mutate(max_s_r = 0.95 * max(X)) %>% 
    ungroup() %>% 
    filter(X > max_s_r) %>% 
    group_by(species, R) %>% 
    slice_head(n = 1) %>% 
    ungroup() %>%
    select(time, species, R) %>%
    filter(time > 2) %>% 
    complete(species, R, fill = list(time = 0)) %>% 
    ggplot(aes(x = species, y = time, fill = R)) +
    geom_col(position = "dodge", color = "black") +
    scale_fill_brewer(palette = "Set1") +
    coord_flip() +
    ggtheme_plot() +
    labs(y = "Recovery time",
         x = "Species") +
    guides(fill = guide_legend(title = "")) +
    theme(legend.justification = c(1, 1),
          legend.position = c(1, 1)))

lazy_ggsave(plot = time_to_k,
            file = "time_to_k",
            height = 10,
            width = 12)

(recovery_plot <- summarize_results %>% 
    ggplot(aes(x = time, y = x_mean, color = species, fill = species)) +
    geom_ribbon(aes(ymin = x_2.5, ymax = x_97.5), alpha = 0.25, size = 0.1) +
    geom_line(size = 1) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 1)) +
    labs(x = "Time (years)", y = "Normalized biomass (B / K)") +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    theme(strip.background = element_blank(),
          legend.position = "bottom",
          text = element_text(size = 10)) +
    guides(color = guide_legend(title = "Species",
                                ncol = 2,
                                label.theme = element_text(face = "italic",
                                                           size = 8)),
           fill = guide_legend(title = "Species")) +
    facet_grid(SSP~R))


lazy_ggsave(plot = recovery_plot,
           file = "recovery_plot",
           height = 20,
           width = 16)


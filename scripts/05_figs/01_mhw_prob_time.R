library(here)
library(tidyverse)

MHW_models <- readRDS(here::here("data", "MHW_models.rds"))

simulations <- MHW_models %>% 
  filter(str_detect(dep_var, "Mean")) %>% 
  mutate(mhw = map(model, sim_mhw, nsim = 1e4, df = T)) %>% 
  select(SSP, mhw) %>% 
  unnest(cols = mhw) %>% 
  pivot_longer(cols = contains("sim"), values_to = "MHW", names_to = "sim")

ggplot(data = simulations,
       mapping = aes(x = year, y = MHW, color = SSP, fill = SSP)) +
  stat_summary(geom = "line", fun = "mean") +
  theme_bw() +
  startR::ggtheme_plot() +
  scale_color_brewer(palette = "Set1")

simulations %>% 
  group_by(year, SSP) %>% 
  summarize(n = sum(MHW) / length(MHW)) %>% 
  ggplot(aes(x = year, y = SSP, fill = n)) +
  geom_point(shape = 21)

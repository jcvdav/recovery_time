################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(startR)
library(here)
library(tidyverse)

# Load data --------------------------------------------------------------------
cona <- read.csv(#here("data", "conapesca_baja_ts.csv"),
  here("data", "conapesca_baja_ts_until_2013.csv"),
  stringsAsFactors = F) %>% 
  group_by(taxa) %>% 
  mutate(n = n()) %>%  
  ungroup() %>% 
  filter(n > 5,
         !taxa == "Sandbass")

## VISUALIZE ###################################################################
# Build plot -------------------------------------------------------------------
(catches_ts <- ggplot(data = cona,
                      mapping = aes(x = year_cut, y = landed_weight / 1e3)) +
   geom_line() +
   geom_point(fill = "steelblue", color = "black", size = 2, shape = 21) +
   facet_wrap(~taxa, scales = "free_y") +
   ggtheme_plot() +
   labs(x = "Year", y = "Landings\n(Metric Tones)"))


## EXPORT ######################################################################

# Export -----------------------------------------------------------------------
lazy_ggsave(plot = catches_ts,
            filename = "catches_ts",
            height = 10,
            width = 24)

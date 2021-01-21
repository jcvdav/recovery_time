# Modeling probability of future MHWs for Baja pixels from CMIP6 Projections
# Written by Dave S
# December 2020


# Packages ----------------------------------------------------------------

library(tidyverse)
library(visreg)
library(patchwork)


# Get the CMIP data ------------------------------------------------------------

getDeets <- function(x) {
  bits <- unlist(strsplit(x, "/"))
  fName <- bits[length(bits)]
  bits1 <- unlist(strsplit(fName, "_"))
  yrs <- bits1[length(bits1)]
  yrs <- unlist(strsplit(yrs, ".nc"))
  yrs <- unlist(strsplit(yrs, "-"))
  ssp <- bits[grep("ssp", bits)[1]]
  return(list(model = bits1[!grepl("tos|Oday|.nc", bits1)], ssp = ssp, syr = yrs[1], eyr = yrs[2]))
}

f <- dir(pattern = "CumInt_", full.names = TRUE, recursive = TRUE)
gMHW <- function(x) {
  bits <- getDeets(x) # Doesn't work quite like it should but it does the trick...with a little juggling
  d <- read.csv(x) %>% 
    mutate(SSP = bits$model[2],# Yes, I know this looks funny, but it works
           Model = gsub(".csv", "", bits$model[3])) %>% # Yes, I know this looks funny, but it works
    select(SSP, Model, everything())
}
lapply(f, gMHW) %>% 
  bind_rows() %>% 
  filter(Year > 2019) %>% 
  pivot_wider(names_from = SSP, values_from = Cum_Int, values_fill = 0) %>% 
  data.frame() %>% 
  write.csv("Projected MHW Cumulative Intensity.csv", row.names = FALSE)

# Plots ------------------------------------------------------------

oisst <- read.csv(here::here("data", "MHW_Dave", "OISST_Cum_Int.csv")) %>% 
  filter(Year == 2014 | Year == 2015)
cmip <- lapply(f, gMHW) %>% 
  bind_rows() %>% 
  filter(Year > 2019)
ggplot(cmip, aes(x = Year, y = Cum_Int, colour = SSP)) +
  geom_line() +
  geom_hline(yintercept = oisst$Cum_Int) +
  facet_wrap(vars(Model))


# Models ------------------------------------------------------------------

dat <- cmip %>% 
  mutate(Exceed2014 = ifelse(Cum_Int > oisst$Cum_Int[1], 1, 0),
         Exceed2015 = ifelse(Cum_Int > oisst$Cum_Int[2], 1, 0),
         ExceedMean = ifelse(Cum_Int > mean(oisst$Cum_Int), 1, 0))
# 2014
ssp126_2014 <- glm(Exceed2014 ~ Year, family = binomial, data = filter(dat, SSP == "ssp126"))
ssp245_2014 <- glm(Exceed2014 ~ Year, family = binomial, data = filter(dat, SSP == "ssp245"))
ssp585_2014 <- glm(Exceed2014 ~ Year, family = binomial, data = filter(dat, SSP == "ssp585"))

# 2015
ssp126_2015 <- glm(Exceed2015 ~ Year, family = binomial, data = filter(dat, SSP == "ssp126"))
ssp245_2015 <- glm(Exceed2015 ~ Year, family = binomial, data = filter(dat, SSP == "ssp245"))
ssp585_2015 <- glm(Exceed2015 ~ Year, family = binomial, data = filter(dat, SSP == "ssp585"))

# Mean
ssp126_Mean <- glm(ExceedMean ~ Year, family = binomial, data = filter(dat, SSP == "ssp126"))
ssp245_Mean <- glm(ExceedMean ~ Year, family = binomial, data = filter(dat, SSP == "ssp245"))
ssp585_Mean <- glm(ExceedMean ~ Year, family = binomial, data = filter(dat, SSP == "ssp585"))

a <- dat %>% 
  group_by(SSP) %>% 
  nest() %>% 
  expand_grid(fml = c("ExceedMean ~ Year", "Exceed2015 ~ Year", "Exceed2014 ~ Year")) %>% 
  mutate(model = map2(.x = fml, .y = data, .f = glm, family = "binomial"))


# Simulate one time series
sFun <- function(mod) { # Simulate observations from one of these models (mod) for a
  yr <- mod$data$Year
  n_yr <- length(unique(yr))
  simDat <- simulate(ssp585_Mean)$sim_1
  d <- data.frame(Year = yr[1:n_yr], Pred = simDat[1:n_yr])
  return(d)
}

# Example
sFun(ssp126_Mean)

##### Sim wrap


models <- dat %>% 
  group_by(SSP) %>% 
  nest() %>% 
  expand_grid(dep_var = c("ExceedMean", "Exceed2015", "Exceed2014")) %>%
  mutate(fml = paste0(dep_var, " ~ Year")) %>% 
  mutate(model = map2(.x = fml, .y = data, .f = glm, family = "binomial")) %>% 
  select(SSP, dep_var, model)

saveRDS(models, here::here("data", "MHW_models.rds"))




















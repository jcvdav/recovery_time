#Code developed by Nur Arafeh Dalmau, The University of Queensland
#Project: Biophysical design principles for MPAs, Southern California Bight ecoregion
#Notes_ Check R and stats books at: EPDF for books
# Comments by Dave S; 30 September 2020 - edited lines marked ****
#Update by Nur AD; 1 October 2020 - edited lines marked !!! (Line 53, 79)
#Following Dave's comments- Updated by Nur 10/07/20
# Recoded by Dave S 13/14 October 2020 - to check
#Final version developed by Dave November 2020

library(MuMIn) #To see r2
library (lme4)
library(tidyverse)
library (lattice)
library (glmmTMB)
library (DHARMa)
library(lmerTest)
library(MASS)
library(car)
library(ggfortify) # New Library for diagnostics



# Data prep ---------------------------------------------------------------
###To simplify the analysis and be consistent with the logistic growth model patially implicit discrete-time logistic growth model with a reserve and fishing zones. I combined MPA and fishin as "fishing zone" and no-take as "reserved".
#Load data
MHWs <- read.csv("Final_Data_MPAs_combined_thresold_none.csv", stringsAsFactors = TRUE) %>% #**** Let's make strings be factors rather than character
  dplyr::select(Site, MPA, Thresold, Program, year, Lat, Long, Group_sp, Density_m2, Genusspeices, cumm_int) #**** Note that there are many functions called select, so we tell R what package we need to refer to

#Filter by lattitude and longitude. Based on a community composition analysis done in California for the 
#Marine Life protection act. I filtered south of Los Angeles, and only kept Catalina and Sant Clemente Island. 
#Got rid of year 2016, as year after is not sifnificant and it is out of scope for the purpose of our analysis

#MHWs <- MHWs %>% filter(year < 2016)


MHWs <- MHWs %>% filter(Lat < 33.8, Long > -118.7, year < 2016)


#Estimate the mean density grouped by species group. Example Haliotis species = abalone
Groupsp <- MHWs %>% group_by (Site, MPA, Thresold, Program, year, Lat, Long, Group_sp, cumm_int) %>%
  summarise(Density_m2 = sum(Density_m2)) %>% 
  arrange(Site, MPA, Thresold, year, Group_sp)
# rename(Density_m2 = `mean(Density_m2)`) 


# Data munging ------------------------------------------------------------

# Loop through each species group, standardise by site, compute delta and consolidate
  for(i in levels(Groupsp$Group_sp)) { # For each species
    out <- list() # Empty list to catch output
    d <- filter(Groupsp, Group_sp == i) %>% 
      droplevels() # Just the factor levels where this species is recorded
    for(j in levels(d$Site)) { # For each site
      dj <- filter(d, Site == j) %>% 
        mutate(sD = NA, dD = NA, Delta = NA, Before = NA) # Make a columns for scaled density, delta scaled Density, raw delta density and rw density in the year #********
      if(nrow(dj) > 1) { # Figure out which rows can be scaled - NOTE that we are scaling by taxon for each site
        for(k in 2:nrow(dj)) { # Only if there are at least two rows - scale requirs sd, so n >=2
          if(dj$year[k] - dj$year[k-1] == 1 & # Only run for consecutive years AND
            (dj$Density_m2[k] != 0 | dj$Density_m2[k-1] != 0)) { # Where both values are not zero (to avoid over-interpreting paired zeroes)
              dj$sD[k] <- dj$sD[k-1] <- 1 # Insert a flag for which rows are OK to include in scaled data
              }
        }
        dj <- dj[!is.na(dj$sD),] # Dump rows we don't need
        if(nrow(dj) > 1) { # Only where there are some data left, scale and compute difference
          dj$sD <- as.vector(scale(dj$Density_m2)) # Scale if there are enough data at the site
          for(kk in 2:nrow(dj)) {
            if(dj$year[kk] - dj$year[kk-1] == 1) {
              dj$dD[kk] <- dj$sD[kk] - dj$sD[kk-1] # Compute the difference in scaled density
              dj$Delta[kk] <- dj$Density_m2[kk] - dj$Density_m2[kk-1] # Compute the difference in raw density #***************
              dj$Before[kk] <- dj$Density_m2[kk-1] # Keep density before any change #***************
            }
            }
          } else {
            dj$dD <- NA # If there are not at least two rows, we cannot scale the data, so we dump them
            }
        }
      dj <- dj[!is.na(dj$dD),] # Kill rows with NAs
      if(nrow(dj) > 0) {
        dO <- paste0("out$", gsub(" ", "", i), "_", gsub(" ", "", j), " <- dj") # Write output to the output lsit, after stripping spaces from names
          eval(parse(text = dO))
          }
      }
    dO <- paste0(gsub(" ", "", i), " <- bind_rows(out)") # Write the data for the taxon to a tibble
      eval(parse(text = dO))
    }


# Functions ---------------------------------------------------------------
# Builds a plotting data frame for a (g)lm (mod), data (d), number of steps in the output sequence for continuous variables (no.out) and alpha
getFit <- function(mod, d, no.out = 250, alpha = 0.05) {
  crit <- -qnorm(alpha/2) # Approximate critical value to convert StdErr into CI
  linv <- family(mod)$linkinv # Extract the inverse-link function...vital for glms
  m <- formula(mod,fixed.only = TRUE)[-2] # Fixed effects from mod
  # Build prediction data frame
  mc <- as.character(m)[2] # Make the formula character, instead
  # Split out fixed effects terms, ignoring interactions
  fs <- unlist(strsplit(mc, " * "))[which(unlist(lapply(unlist(strsplit(mc, " * ")), nchar)) > 1)]
  if(length(grep(":", fs)) > 0) fs <- fs[-grep(":", fs)] 	
  fs <- gsub("\\(", "", gsub("\\)", "", fs)) # [DS] New line: gets rid of braces
  # Figure out which effects are factor and which are continuous, and fill, as necessary
  out <- list()
  for(i in 1:length(fs)) {
    if(eval(parse(text = paste0("with(d,is.factor(", fs[i], "))")))) {
      out[[fs[i]]] <- eval(parse(text = paste0("with(d, ", fs[i], " <- levels(", fs[i], "))")))
    } else {
      out[[fs[i]]] <- eval(parse(text = paste0("with(d, ", fs[i], " <- seq(min(", fs[i], "), max(", fs[i], "), length.out = no.out))")))	
    }
  }
  # Expand out and predict from the model using the new predictors
  p <- expand.grid(out) # Make preditor data frame
  fits <- predict(mod, newdat = p, se.fit = TRUE) %>% # Predict from the model for the new data we created
    as.data.frame() %>% # Make it a data frame, rather than a list
    dplyr::select(fit, se.fit) %>%  # Pick just the fits and their std errors
    mutate(se.hi =  fit + se.fit, # Upper error bar (SE)
           se.lw =  fit - se.fit, # Lower error bar (SE)
           upr = fit + crit*se.fit, # Approx upper (1-alpha)% conf limit for fit
           lwr = fit - crit*se.fit # Approx lower (1-aplha)% conf limit for fit
    ) %>% 
    dplyr::select(-se.fit) # Remove the std error
  # Apply the inverse link function to ensure that we are in the orignal data space
  Fits <- as.data.frame(lapply(fits, linv)) 
  # Output the result as a prediction data frame
  return(cbind(p, Fits))
  }


# Model and plot ----------------------------------------------------------
#### We excluded fish and lobster, as they are mobile and less affected by MHWs. They can shift their distribution poleward or deeper.


D <- Sea_Urchin
m1 <- lm(dD ~ MPA * Thresold,
         data = D) # Fit the same model for each taxon
print(summary(m1)) # Print a summary
print(anova(m1, test = "F")) # Print an anova table
autoplot(m1)
m2 <- step(m1)
print(summary(m2))
pdat <- getFit(m1, D) # Build a plot
ggplot(pdat, aes(x = Thresold, colour = MPA)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), 
                width = .09,
                position = position_dodge(.3)) +
  geom_point(aes(y = fit), 
             size = 2,
             position = position_dodge(.3))



D <- Abalone
m1 <- lm(dD ~ MPA * Thresold,
         data = D) # Fit the same model for each taxon
print(summary(m1)) # Print a summary
print(anova(m1, test = "F")) # Print an anova table
autoplot(m1)
m2 <- step(m1)
print(summary(m2))
pdat <- getFit(m1, D) # Build a plot
ggplot(pdat, aes(x = Thresold, colour = MPA)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), 
                width = .09,
                position = position_dodge(.3)) +
  geom_point(aes(y = fit), 
             size = 2,
             position = position_dodge(.3))



D <- Sea_Cucumber
m1 <- lm(dD ~ MPA * Thresold,
         data = D) # Fit the same model for each taxon
print(summary(m1)) # Print a summary
print(anova(m1, test = "F")) # Print an anova table
autoplot(m1)
m2 <- step(m1)
print(summary(m2))
pdat <- getFit(m1, D) # Build a plot
ggplot(pdat, aes(x = Thresold, colour = MPA)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), 
                width = .09,
                position = position_dodge(.3)) +
  geom_point(aes(y = fit), 
             size = 2,
             position = position_dodge(.3))


#### MPA and Thresold interaction makes the model worst. We only use Thresold in next models. 



### Now we need to estimate the density change for each group in a non-MHW year vs a MHW year

# *** Worked example using Sea Urchins ***    -----------------------------

D <- Sea_Urchin
m_scDelta <- lm(dD ~ Thresold,
         data = D) # Model of effect of MHW on scaled density
print(summary(m_scDelta)) # Print a summary
print(anova(m_scDelta)) # Print an anova table

pdat <- getFit(m_scDelta, D) # Build a plot
ggplot(pdat, aes(x = Thresold)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), 
                width = .09,
                position = position_dodge(.3)) +
  geom_point(aes(y = fit), 
             size = 2,
             position = position_dodge(.3))
  # There is a clear and significant effect, with the year-to-year change being large, negative and significant after a MHW

  # The effects on scaled density change can be extracted from pdat
    pdat$fit # So, the year-to-year change in density without an MHW is close to zero SDs (in fact, not significantly different from zero SDs), whereas the difference across an MHW is close to -0.58 SDs
      # So, the estimated year-to-year difference in the presence of a MHW is -0.54 SDs
  # Next, we simply convert back from scaled year-to-year change to raw year-to-year change
    # Extract Mean and SD of raw change
      Mag <- Sea_Urchin %>% group_by (Site) %>% summarise(Mean_D = mean(Delta), sdD = sd(Delta), mnBefore = mean(Before)) %>% na.omit() 
    # Use that to compute raw change per site
      diff_SeaUrch <- Mag %>% 
        mutate(NoMHW = 100*((pdat$fit[1] * sdD) + Mean_D)/mnBefore,
               MHW = 100*((pdat$fit[2] * sdD) + Mean_D)/mnBefore)
      diff_SeaUrch
      # These are the % change in mean density from before to after in the presence of MHW and not
      diff_SeaUrch %>% summarise(NoMHW = mean(NoMHW), MHW = mean(MHW))
    # So the average density change in the absence of MHWs is -19% whereas that in the presence of MHWs is -72%
      
####Rest of species      
 
D <- Abalone
m_scDelta <- lm(dD ~ Thresold,
                      data = D) # Model of effect of MHW on scaled density
print(summary(m_scDelta)) # Print a summary
print(anova(m_scDelta)) # Print an anova table
      
pdat <- getFit(m_scDelta, D) # Build a plot
ggplot(pdat, aes(x = Thresold)) +
    geom_errorbar(aes(ymin = lwr, ymax = upr), 
                      width = .09,
                      position = position_dodge(.3)) +
    geom_point(aes(y = fit), 
                   size = 2,
                   position = position_dodge(.3))

  # The effects on scaled density change can be extracted from pdat
  pdat$fit 
  # Next, we simply convert back from scaled year-to-year change to raw year-to-year change
    # Extract Mean and SD of raw change
      Mag_Abalone <- Abalone %>% group_by (Site) %>% summarise(Mean_D = mean(Delta), sdD = sd(Delta), mnBefore = mean(Before)) %>% na.omit() 
  # Use that to compute raw change per site
      diff_Abalone <- Mag_Abalone %>% 
        mutate(NoMHW = 100*((pdat$fit[1] * sdD) + Mean_D)/mnBefore,
               MHW = 100*((pdat$fit[2] * sdD) + Mean_D)/mnBefore)
      diff_Abalone
  # These are the % change in mean density from before to after in the presence of MHW and not
   diff_Abalone %>% summarise(NoMHW = mean(NoMHW), MHW = mean(MHW))
  # So the average density change in the absence of MHWs is 4.62% whereas that in the presence of MHWs is -59.1%  
   
   
D <- Sea_Cucumber
m_scDelta <- lm(dD ~ Thresold,
                   data = D) # Model of effect of MHW on scaled density
print(summary(m_scDelta)) # Print a summary
print(anova(m_scDelta)) # Print an anova table
   
pdat <- getFit(m_scDelta, D) # Build a plot
  ggplot(pdat, aes(x = Thresold)) +
    geom_errorbar(aes(ymin = lwr, ymax = upr), 
                   width = .09,
                   position = position_dodge(.3)) +
    geom_point(aes(y = fit), 
                size = 2,
                position = position_dodge(.3))

   # The effects on scaled density change can be extracted from pdat
   pdat$fit 
   # Next, we simply convert back from scaled year-to-year change to raw year-to-year change
   # Extract Mean and SD of raw change
   Mag_Sea_Cucumber <- Sea_Cucumber %>% group_by (Site) %>% summarise(Mean_D = mean(Delta), sdD = sd(Delta), mnBefore = mean(Before)) %>% na.omit() 
   # Use that to compute raw change per site
   diff_Sea_Cucumber <- Mag_Sea_Cucumber %>% 
     mutate(NoMHW = 100*((pdat$fit[1] * sdD) + Mean_D)/mnBefore,
            MHW = 100*((pdat$fit[2] * sdD) + Mean_D)/mnBefore)
   diff_Sea_Cucumber
   # These are the % change in mean density from before to after in the presence of MHW and not
   diff_Sea_Cucumber %>% summarise(NoMHW = mean(NoMHW), MHW = mean(MHW))
   # So the average density change in the absence of MHWs is -18.62% whereas that in the presence of MHWs is -67.3%              
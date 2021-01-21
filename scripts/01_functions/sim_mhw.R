sim_mhw <- function(mod, nsim){
  n_yr <- length(unique(mod$data$Year))
  simulated <- simulate(object = mod, nsi = nsim) %>%
    head(n_yr) %>% 
    as.matrix()
  
  return(simulated)
}

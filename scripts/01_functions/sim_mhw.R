sim_mhw <- function(mod, nsim, df = FALSE){
  # HAVE TO MODIFY THIS FUNCTION TO GET
  all_years <- mod$data$Year
  unique_years <- unique(all_years)
  
  get_random_index <- function(unique){
    indices <- which(all_years == unique)
    index <- sample(indices, 1)
    return(index)
  }
  
  indices <- map_dbl(unique_years, get_random_index)
  simulated <- simulate(object = mod, nsi = nsim) %>%
    as.matrix()
  
  simulated <- simulated[indices,]
  rownames(simulated) <- unique_years
  
  if(df){
    simulated <- as.data.frame(simulated) %>% 
      rownames_to_column("year") %>% 
      mutate(year = as.numeric(year))
  }
  
  return(simulated)
}

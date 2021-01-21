## Iterate through time for each species
sim_reserve <- function(nyears, r, K, fmsy, R, delta, model, nsim) {
  shocks <- sim_mhw(model, nsim)
  x0 <- 0.2 * K
  X <- matrix(nrow = nyears, ncol = nsim)
  colnames(X) <- paste0("sim_", 1:nsim)
  X[1,] <- x0

  for(i in 2:nyears) {
    X[i,] <- grow(x = X[i - 1,], r = r, K = K, fmsy = fmsy, R = R, shock = shocks[i,], delta = delta)
  }
  
  
  results <- as.data.frame(X) %>% 
    mutate(time = 1:nyears) %>% 
    gather(sim, X, -time)
  
  return(results)
}

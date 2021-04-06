## logistic growth function with harvesting and
## a spatially implicit reserve
grow <- function(x, r, K, fmsy, R, shock, delta) {
  
  # x[shock == 1] <- (1 - delta) * x[shock == 1]
  
  res <- ((1 - (delta * shock)) * x) + (r * x * (1 - (x / K))) - ((1 - R) * x * fmsy)
  
  res <- pmax(res, 0)
  return(res)
}

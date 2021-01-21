## logistic growth function with harvesting and
## a spatially implicit reserve
grow <- function(x, r, K, fmsy, R, shock, delta) {
  
  x[shock == 1] <- (1 - delta) * x[shock == 1]
  x <- pmax(x, 0)
  
  res <- x + (r * x * (1 - (x / K))) - ((1 - R) * x * fmsy)
  return(res)
}

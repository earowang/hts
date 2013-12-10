window.gts <- function(y, ...) {
  # Select a snapshot of hts or gts
  y$bts <- window(y$bts, ...)
  tsp(y) <- tsp(y)
  return(y)
}

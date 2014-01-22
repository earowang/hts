window.gts <- function(x, ...) {
  # Select a snapshot of hts or gts
  x$bts <- window(x$bts, ...)
  tsp(x) <- tsp(x)
  return(x)
}

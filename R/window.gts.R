window.gts <- function(x, ...) {
  # Select a snapshot of hts or gts
  x$bts <- stats::window(x$bts, ...)
  tsp(x) <- stats::tsp(x)
  return(x)
}

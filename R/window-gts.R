#' Time window of a gts object
#' 
#' Extracts a subset of the time series from a grouped time series object.
#' 
#' 
#' @param x An object of class \code{\link[hts]{gts}}.
#' @param ... All other arguments are passed to \code{\link[stats]{window.ts}}.
#' @author Rob J Hyndman
#' @keywords ts
#' @method window gts
#' @examples
#' 
#' window(htseg2, start = 2000, end = 2001)
#' 
#' @export
window.gts <- function(x, ...) {
  # Select a snapshot of hts or gts
  x$bts <- stats::window(x$bts, ...)
  tsp(x) <- stats::tsp(x)
  return(x)
}

#' Extract selected time series from a gts object
#' 
#' The time series from selected levels of a hierarchical/grouped time series
#' or a forecasted hierarchical/grouped time series are returned as a
#' multivariate time series.
#' 
#' 
#' @param y An object of class \code{{gts}}.
#' @param levels Integer(s) or string(s) giving the specified level(s).
#' @param forecasts If \code{y} contains forecasts and historical data, then
#' \code{forecasts} indicates whether to return the forecasts or the historical
#' data. Otherwise it is ignored.
#' @author Earo Wang
#' @seealso \code{\link[hts]{allts}}
#' @keywords ts
#' @examples
#' 
#' aggts(htseg1, levels = c(0, 2))
#' aggts(infantgts, levels = "State")
#' 
#' @export aggts
aggts <- function(y, levels, forecasts = TRUE) {
  # 1. Display all time series from top to bottom.
  # 2. Bottom-up method.
  #
  # Args:
  #   y*: hts & gts objects.
  #   levels: hts levels (gts groups) can be specified by users. Default is all
  #     starting with level 0.
  #
  # Returns:
  #   The time series selected by users.
  #
  # Error Handling:
  if (!is.gts(y)) {
    stop("Argument y must be either a hts or gts object.", call. = FALSE)
  }

  if (!forecasts) {
    y$bts <- y$histy
  }

  if (is.hts(y)) {
    gmat <- GmatrixH(y$nodes)
    labels <- y$labels
  } else {
    gmat <- y$groups
    labels <- c("Total", y$labels, list(colnames(y$bts)))
  }

  if (missing(levels)) {
    # Return all levels of the time series
    levels <- 1L:nrow(gmat)
  } else {
    if (is.character(levels)) {  # Strings consistent with groups names
      levels <- which(names(y$labels) %in% levels)
    }
    # Return the specified levels
    levels <- as.integer(levels) + 1L
  }

  # A function to aggregate the bts
  rSum <- function(x) rowsum(t(y$bts), gmat[x, ], reorder = FALSE, na.rm = TRUE)

  ally <- lapply(levels, rSum)
  # Convert lists to matrices
  ally <- matrix(unlist(sapply(ally, t)), nrow = nrow(y$bts))

  colnames(ally) <- unlist(labels[levels])
  tsp.y <- stats::tsp(y$bts)
  ally <- ts(ally, start = tsp.y[1L], frequency = tsp.y[3L])
  # Assign other attributes
  class(ally) <- class(y$bts)
  attr(ally, "msts") <- attr(y$bts, "msts")
  return(ally)
}


# A wrapper for aggts


#' Extract all time series from a gts object
#' 
#' The time series from all levels of a hierarchical/grouped time series or a
#' forecasted hierarchical/grouped time series are returned as a multivariate
#' time series.
#' 
#' 
#' @param y An object of class \code{\link[hts]{gts}}.
#' @param forecasts If \code{y} contains forecasts and historical data, then
#' \code{forecasts} indicates whether to return the forecasts or the historical
#' data. Otherwise it is ignored.
#' @author Rob J Hyndman
#' @seealso \code{\link[hts]{aggts}}
#' @keywords ts
#' @examples
#' 
#' allts(htseg1)
#' 
#' @export allts
allts <- function(y, forecasts = TRUE) {
  if (!is.gts(y)) {
    stop("Argument y must be either a hts or gts object.", call. = FALSE)
  }
  aggts(y = y, forecasts = forecasts)
}

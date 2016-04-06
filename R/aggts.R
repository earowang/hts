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
    stop("Argument y must be either a hts or gts object.")
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
allts <- function(y, forecasts = TRUE) {
  if (!is.gts(y)) {
    stop("Argument y must be either a hts or gts object.")
  }
  aggts(y = y, forecasts = forecasts)
}

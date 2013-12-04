allts <- function(y, levels = NULL) {
  # Display all time series from top to bottom.
  #
  # Args:
  #   y*: hts & gts objects.
  #   levels: hts levels (gts groups) can be specified by users. Default is all
  #     starting with level 0.
  # 
  # Returns:
  #   The time series selected by users.
  #
  # ToDo:
  #   1. Add argument forecast = TRUE/FALSE
  #
  # Error Handling:
  if (!is.hts(y) && !is.gts(y)) {
    stop("Argument y must be either a hts or gts object.")
  }

  if (is.hts(y)) {
    gmat <- GmatrixH(y$nodes)
    labels <- y$labels
  } else {
    gmat <- y$groups
    labels <- c("Total", y$gnames, list(colnames(y$bts)))
  }

  # A function to aggregate the bts
  rSum <- function(x) rowsum(t(y$bts), gmat[x, ])

  if (is.null(levels)) {
    # Return all levels of the time series
    levels <- 1L:nrow(gmat)
    ally <- lapply(levels, rSum)
  } else {
    # Return the specified levels
    levels <- as.integer(levels) + 1L
    ally <- lapply(levels, rSum)
  }
  # Convert lists to matrices
  ally <- matrix(unlist(sapply(ally, t)), nrow = nrow(y$bts))
  colnames(ally) <- unlist(labels[levels])
  tsp.y <- tsp(y$bts)
  ally <- ts(ally, start = tsp.y[1L], frequency = tsp.y[3L])
  return(ally)
}

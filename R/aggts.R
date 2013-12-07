aggts <- function(y, levels) {
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
    # Return the specified levels
    levels <- as.integer(levels) + 1L
  }

  if (!is.null(y$f)) {
    obj <- y$f
  } else {
    obj <- y$bts
  }

  # A function to aggregate the bts
  rSum <- function(x) rowsum(t(obj), gmat[x, ])

  ally <- lapply(levels, rSum)
  # Convert lists to matrices
  ally <- matrix(unlist(sapply(ally, t)), nrow = nrow(obj))

  colnames(ally) <- unlist(labels[levels])
  tsp.y <- tsp(obj)
  ally <- ts(ally, start = tsp.y[1L], frequency = tsp.y[3L])
  return(ally)
}

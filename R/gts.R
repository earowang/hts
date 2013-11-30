gts <- function(y, groups, gnames = rownames(groups)) {
  # Construct the grouped time series.
  #
  # Args:
  #   y*: The bottom time series assigned by the user.
  #   groups: A matrix contains the distinctive No. for each group at each row.
  #   gnames: Specify the group names.
  #
  # Returns:
  #   A grouped time series.
  #
  # Error handling:
  if (!is.ts(y)) {
    stop("Agrument y must be a time series data.")
  }
  if (ncol(y) <= 1L) {
    stop("Argument y must be a multiviate time series.")
  }
  if (any(is.na(y))) {
    stop("Argument y must not have missing values.")
  }
  if (!is.matrix(groups)) {
    stop("Argument groups must be a matrix.")
  }
  # Check whether groups is unique
  bgroup <- unique(apply(groups, 2, paste, collapse = ""))
  if (ncol(groups) != ncol(y) && length(bgroup) != ncol(y)) {
    stop("Argument groups is misspecified.")
  }

  if (missing(groups)) {
    groups <- matrix(1L:ncol(y), nrow = 1L, byrow = TRUE)
  } else {
    groups <- as.matrix(groups)
  }
}


# A function to convert groups to gmatrix
GmatrixG <- function(xmat) {
  if (is.character(xmat)) {
    # Convert character to numeric
    gmat <- t(apply(xmat, 1, function(x) as.numeric(factor(x, unqiue(x)))))
  } else {
    gmat  <- xmat
  }
  # Insert the first row
  gmat <- rbind(rep(1, ncol(xmat)), gmat)
  # Insert the bottom row
  gmat <- rbind(gmat, seq(1, ncol(xmat)))
  return(gmat)
}

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

  # Construct gmatrix
  if (missing(groups)) {
    gmat <- matrix(c(rep(1L, ncol(y)), seq(1L, ncol(y))), nrow = 2L, 
                   byrow = TRUE)
  } else {
    groups <- as.matrix(groups)
    gmat <- GmatrixG(groups)  # GmatrixG() defined below
  }

  # Construct gnames
  if (is.null(gnames)) {
    message("Agrument gnames is missing and the default labels are used.")
    gnames <- c("Total", ifelse(nrow(gmat) == 2L, "Bottom", 
                c(paste("Group", LETTERS[1L:(nrow(gmat) - 2L)]), 
                "Bottom")))
  } else {
    gnames <- c("Total", gnames, "Bottom")
  }

  colnames(gmat) <- colnames(y)
  rownames(gmat) <- gnames

  return(structure(list(bts = y, groups = gmat, gnames = gnames), 
                   class = "gts"))
}


# A function to convert groups to gmatrix
GmatrixG <- function(xmat) {
  if (is.character(xmat)) {
    # Convert character to numeric
    gmat <- t(apply(xmat, 1, function(x) as.numeric(factor(x, unqiue(x)))))
  } else {
    gmat  <- xmat
  }
  # Insert the first & last rows
  gmat <- rbind(rep(1L, ncol(xmat)), gmat, seq(1L, ncol(xmat)))
  return(structure(gmat, class = "gmatrix"))
}

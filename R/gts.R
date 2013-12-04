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
    stop("Argument y must be a time series data.")
  }
  if (ncol(y) <= 1L) {
    stop("Argument y must be a multivariate time series.")
  }
  if (any(is.na(y))) {
    stop("Argument y must not have missing values.")
  }
  if (missing(groups)) {
    groups <- matrix(c(rep(1L, ncol(y)), seq(1L, ncol(y))), nrow = 2L, 
                   byrow = TRUE)
    gmat <- groups
  } else if (!is.matrix(groups)) {
    stop("Argument groups must be a matrix.")
  } else {
    # Construct gmatrix
    groups <- as.matrix(groups)
    gmat <- GmatrixG(groups)  # GmatrixG() defined below
  }
  # Check whether groups is unique
  bgroup <- unique(apply(groups, 2, paste, collapse = ""))
  if (ncol(groups) != ncol(y) && length(bgroup) != ncol(y)) {
    stop("Argument groups is misspecified.")
  }

  # Construct gnames
  if (nrow(gmat) == 2L) {
    name.list <- NULL
  } else if (is.null(gnames)) {
    message("Argument gnames is missing and the default labels are used.")
    gnames <- paste0("G", 1L:(nrow(gmat) - 2L))
  } 
  colnames(gmat) <- colnames(y)
  rownames(gmat) <- c("Total", gnames, "Bottom")

  # Keep the names at each group
  if (nrow(gmat) > 2L) {
    times <- Mlevel(groups)
    full.groups <- mapply(rep, as.list(gnames), times, SIMPLIFY = FALSE)
    subnames <- apply(groups, 1, unique)
    if (is.matrix(subnames)) {
      # Convert a matrix to a list
      subnames <- split(subnames, rep(1L:ncol(subnames), each = nrow(subnames)))
    } 
    name.list <- mapply(paste0, full.groups, "/", subnames, SIMPLIFY = FALSE)
    names(name.list) <- paste("Group", 1L:(nrow(gmat) - 2L))
  }

  return(structure(list(bts = y, groups = gmat, gnames = name.list),
                        class = "gts"))
}


# A function to convert groups to gmatrix
GmatrixG <- function(xmat) {
  if (is.character(xmat)) {
    # Convert character to integer
    gmat <- t(apply(xmat, 1, function(x) as.integer(factor(x, unique(x)))))
  } else {
    gmat  <- xmat
  }
  # Insert the first & last rows
  gmat <- rbind(rep(1L, ncol(xmat)), gmat, seq(1L, ncol(xmat)))
  return(structure(gmat, class = "gmatrix"))
}


# A function to calculate No. of groups at each level
Mlevel <- function(xgroup) {
  m <- apply(xgroup, 1, function(x) length(unique(x)))
  return(m)
}


# A function to check whether it's the "gts" class.
is.gts <- function(xts) {
  is.element("gts", class(xts))
}

# Print "gts" on the screen
print.gts <- function(xts) {
  # ToDo:
  #   1. Add if condition (fcasts) exists
  if (is.hts(xts)) {
    mn <- Mnodes(xts$nodes)
    cat("Hierarchical Time Series \n")
    cat(length(mn), "Levels \n")
    cat("Number of nodes at each level:", mn, "\n")
    cat("Total number of series:", sum(mn), "\n")
  } else {
    cat("Grouped Time Series \n")
    nlevels <- Mlevel(xts$groups)
    cat(length(nlevels), "Levels \n")
    cat("Number of groups at each level:", nlevels, "\n")
    cat("Total number of series:", sum(nlevels), "\n")
  }
  cat("Number of observations per series:", nrow(xts$bts), "\n")
  cat("Top level series:", "\n")
  
  topts <- ts(rowSums(xts$bts), start = tsp(xts$bts)[1L], 
              frequency = tsp(xts$bts)[3L])
  print(topts)
}

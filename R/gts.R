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
  # ToDo:
  #   1. If group > 26, implement "Aa + No" form.
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
    gnames <- c("Total", "Bottom")
  } else if (is.null(gnames)) {
    message("Agrument gnames is missing and the default labels are used.")
    gnames <- c("Total", LETTERS[1L:(nrow(gmat) - 2L)], "Bottom")
  } else {
    gnames <- c("Total", gnames, "Bottom")
  }

  colnames(gmat) <- colnames(y)
  rownames(gmat) <- gnames

  # Keep the names at each group
  times <- apply(groups, 1, function(x) length(unique(x)))
  full.groups <- list(length = length(gnames) - 2L)
  for (i in 2L:(length(gnames) - 1L)) {
    full.groups[[i]] <- rep(gnames[i], times[i])
  }
  subnames <- apply(groups, 1, unique)
  name.list <- mapply(paste0, full.groups, "/", subnames)
  names(name.list) <- gnames

  return(structure(list(bts = y, groups = gmat, gnames = name.list,
                        class = "gts"))
}


# A function to convert groups to gmatrix
GmatrixG <- function(xmat) {
  if (is.character(xmat)) {
    # Convert character to numeric
    gmat <- t(apply(xmat, 1, function(x) as.numeric(factor(x, unique(x)))))
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
  bts <- xts$bts
  cat("Grouped Time Series \n")
  cat(length(Mlevel(xts$groups)), "Levels \n")
  cat("Number of groups at each level:", Mlevel(xts$groups), "\n")
  cat("Total number of series:", sum(Mlevel(xts$groups)), "\n")
  cat("Number of observations per series:", nrow(bts), "\n")
  cat("Top level series:", "\n")
  
  topts <- ts(rowSums(bts), start = tsp(bts)[1L], frequency = tsp(bts)[3L])
  print(topts)
}

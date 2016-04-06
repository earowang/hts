gts <- function(y, groups, gnames = rownames(groups), characters) {
  # Construct the grouped time series.
  #
  # Args:
  #   y*: The bottom time series assigned by the user.
  #   groups: A matrix contains the distinctive No. for each group at each row.
  #   gnames: Specify the group names.
  #   characters: Specify how to split the bottom names in order to generate
  #     the grouping matrix
  #
  # Returns:
  #   A grouped time series.
  #
  # Error handling:
  if (!is.ts(y)) {
    y <- stats::as.ts(y)
  }

  if (ncol(y) <= 1L) {
    stop("Argument y must be a multivariate time series.")
  }
  bnames <- colnames(y)
  nc.y <- ncol(y)
  if (missing(characters)) {
    if (missing(groups)) {
      groups <- matrix(c(rep(1L, nc.y), seq(1L, nc.y)), nrow = 2L,
                     byrow = TRUE)
    } else if (!is.matrix(groups)) {
      stop("Argument groups must be a matrix.")
    } else if (!is.character(groups[1L, ])) { # Check groups numeric matrix
      if (all(groups[1L, ] == 1L)) { # if the first row is all 1's
        groups <- groups[-1L, , drop = FALSE]
      }
      tmp.last <- nrow(groups)
      if (all(groups[tmp.last, ] == seq(1L, nc.y))) { # if the last row is a seq
        groups <- groups[-tmp.last, , drop = FALSE]
      }
    }
    # Check whether groups is unique
    # But R takes so long to check due to the inefficiency with strings
    # bgroup <- unique(apply(groups, 2, paste, collapse = ""))
    # if (ncol(groups) != ncol(y) && length(bgroup) != ncol(y)) {
    #   stop("Argument groups is misspecified.")
    # }
  } else {
    if (length(characters) == 1L) {
      stop("The argument characters must have length greater than one.")
    }
    if (!all(nchar(bnames)[1L] == nchar(bnames)[-1L])) {
      stop("The bottom names must be of the same length.")
    }
    if (any(nchar(bnames) != sum(unlist(characters)))) {
      warning("The argument characters is not fully specified for the bottom names.")
    }
    groups <- CreateGmat(bnames, characters)
  }
  # Construct gmatrix
  gmat <- GmatrixG(groups)  # GmatrixG() defined below

  # Construct gnames
  nr.gmat <- nrow(gmat)
  if (nr.gmat == 2L) {
    name.list <- NULL
  } else if (is.null(gnames)) {
    message("Argument gnames is missing and the default labels are used.")
    gnames <- paste0("G", 1L:(nr.gmat - 2L))
  }
  colnames(gmat) <- bnames
  rownames(gmat) <- c("Total", gnames, "Bottom")

  # Keep the names at each group
  if (nr.gmat > 2L) {
    times <- Mlevel(groups)
    full.groups <- mapply(rep, as.list(gnames), times, SIMPLIFY = FALSE)
    subnames <- apply(groups, 1, unique)
    if (is.matrix(subnames)) {
      # Convert a matrix to a list
      subnames <- split(subnames, rep(1L:ncol(subnames), each = nrow(subnames)))
    }
    name.list <- mapply(paste0, full.groups, "/", subnames, SIMPLIFY = FALSE)
    names(name.list) <- gnames
  }

  return(structure(list(bts = y, groups = gmat, labels = name.list),
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
  nc.xmat <- ncol(xmat)
  gmat <- rbind(rep(1L, nc.xmat), gmat, seq(1L, nc.xmat))
  gmat <- gmat[!duplicated(gmat), , drop = FALSE] # Remove possible duplicated
  return(structure(gmat, class = "gmatrix"))
}


# A function to calculate No. of groups at each level
Mlevel <- function(xgroup) {
  m <- apply(xgroup, 1, function(x) length(unique(x)))
  return(m)
}


# A function to get the inverse of row sums of S matrix
InvS4g <- function(xgroup) {
  mlevel <- Mlevel(xgroup)
  len <- length(mlevel)
  repcount <- mlevel[len]/mlevel
  inv.s <- 1/unlist(mapply(rep, repcount, mlevel, SIMPLIFY = FALSE))
  return(inv.s)
}


# A function to generate the gmatrix based on bottom names
CreateGmat <- function(bnames, characters) {
  total.len <- length(characters)
  sub.len <- c(0L, lapply(characters, length))
  cs <- cumsum(unlist(sub.len))
  int.char <- unlist(characters)
  end <- cumsum(int.char)
  start <- end - int.char + 1L
  tmp.token <- sapply(bnames, function(x) substring(x, start, end))
  # Grab the individual group
  token <- vector(length = total.len, mode = "list")
  for (i in 1L:total.len) {
    token[[i]] <- matrix(, nrow = sub.len[[i + 1L]], ncol = ncol(tmp.token))
  }
  for (i in 1L:total.len) {
    token[[i]][1L, ] <- tmp.token[cs[i] + 1L, ]
    if (sub.len[[i + 1L]] >= 2L) {
      for (j in 2L:sub.len[[i + 1L]]) {
        token[[i]][j, ] <- paste0(token[[i]][j - 1L, ], tmp.token[cs[i] + j, ])
      }
    }
  }
  # Take combinations of any two groups
  cn <- combn(1L:total.len, 2)
  ncl <- ncol(cn)
  groups <- vector(length = ncl, mode = "list")
  for (i in 1L:ncl) {
    bigroups <- list(token[[cn[, i][1L]]], token[[cn[, i][2L]]])
    nr1 <- nrow(bigroups[[1L]])
    nr2 <- nrow(bigroups[[2L]])
    nr <- nr1 * nr2
    tmp.groups <- vector(length = nr1, mode = "list")
    for (j in 1L:nr1) {
      tmp.groups[[j]] <- paste0(bigroups[[1L]][j, ], bigroups[[2L]][1L, ])
      if (nr2 >= 2L) {
        for (k in 2L:nr2) {
          tmp.groups[[j]] <- rbind(tmp.groups[[j]], paste0(bigroups[[1L]][j, ],
                                   bigroups[[2L]][k, ]))
        }
      }
    }
    groups[[i]] <- tmp.groups[[1L]]
    if (nr1 >= 2L) {
      for (h in 2L:nr1) {
        groups[[i]] <- rbind(groups[[i]], tmp.groups[[h]])
      }
    }
  }
  # Combine the individual ones and their combinations
  new.list <- c(token, groups)
  gmatrix <- new.list[[1L]]
  for (i in 2L:length(new.list)) {
    gmatrix <- rbind(gmatrix, new.list[[i]])
  }
  gmatrix <- gmatrix[!duplicated(gmatrix), , drop = FALSE]
  # Remove bottom names if it has
  check <- try(which(gmatrix == bnames, arr.ind = TRUE)[1L, 1L], silent = TRUE)
  if (class(check) != "try-error") {
    gmatrix <- gmatrix[-check, ]
  }
  return(gmatrix)
}


# A function to check whether it's the "gts" class.
is.gts <- function(xts) {
  is.element("gts", class(xts))
}

# Print "gts" on the screen
print.gts <- function(x, ...) {
  if (is.hts(x)) {
    mn <- Mnodes(x$nodes)
    cat("Hierarchical Time Series \n")
    cat(length(mn), "Levels \n")
    cat("Number of nodes at each level:", mn, "\n")
    cat("Total number of series:", sum(mn), "\n")
  } else {
    cat("Grouped Time Series \n")
    nlevels <- Mlevel(x$groups)
    cat(length(nlevels), "Levels \n")
    cat("Number of groups at each level:", nlevels, "\n")
    cat("Total number of series:", sum(nlevels), "\n")
  }

  if (is.null(x$histy)) {  # Original series
    cat("Number of observations per series:", nrow(x$bts), "\n")
    cat("Top level series: \n")
  } else {
    cat("Number of observations in each historical series:",
        nrow(x$histy), "\n")
    cat("Number of forecasts per series:", nrow(x$bts), "\n")
    cat("Top level series of forecasts: \n")
  }
  topts <- ts(rowSums(x$bts, na.rm = TRUE), start = stats::tsp(x$bts)[1L],
              frequency = stats::tsp(x$bts)[3L])
  print(topts)
}

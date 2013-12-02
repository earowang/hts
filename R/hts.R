hts <- function(y, nodes, bnames = colnames(y), characters) {
  # Construct the hierarchical time series.
  # 
  # Args:
  #   y*: The bottom time series assigned by the user. Same lengths and no NA.
  #   nodes: A list contains the number of child nodes for each level except
  #     for the bottom one. If missing, it's assumed to have only one level.
  #   bnames: The names of the bottom time series.
  #   characters: Define how to split the "bnames" in order to construct the
  #     level labels. Otherwise, use the defaul labelling system.
  #
  # Returns:
  #   A hierarchical time series.
  #
  # ToDo:
  #   1. May handle NA's by forecasting them properly.
  #   2. Add summary.hts function.
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
  if (missing(nodes)) {
    nodes <- list(ncol(y))
  } 
  if (!is.list(nodes)) {
    stop("Argument nodes must be a list.")
  } 
  if (length(nodes[[1L]]) != 1L) {
    stop("The root node cannot be empty.")
  }
  if (sum(nodes[[length(nodes)]]) != ncol(y)) {
    stop("The number of terminal nodes is not consistent with the number of 
         bottom time series.")
  }
  if (length(nodes) > 1L) {
    for (i in 1L:(length(nodes) - 1L)) {
      if (sum(nodes[[i]]) != length(nodes[[i + 1]])) {
        error <- paste("The number of nodes for the level", i - 1L, "is not 
                       equal to the number of series of level", i)
        stop(error)
      }
    }
  }

  # Construct the level labels
  if (is.null(bnames) || missing(characters)) {
    message("Since argument bnames/characters are not specified, the default 
            labelling system is used.")
    labels <- HierName(nodes) # HierName() defined below
  } else if (length(characters) != ncol(y) || 
             length(characters) != length(nodes) + 1L) {
    stop("Argument characters is misspecified.")
  } else {
    # Construct labels based on characters
    characters <- as.integer(characters)
    start <- cumsum(characters) - characters + 1L
    end <- cumsum(characters)
    token <- sapply(bnames, function(x) substring(x, start, end))
    labels.mat <- matrix(, nrow = nrow(token), ncol = ncol(token))
    labels.mat[1, ] <- token[1, ]
    for (i in 2L:nrow(labels.mat)) {
      labels.mat[i, ] <- paste0(labels.mat[i - 1, ], labels.mat[i, ])
    }
    rownames(labels.mat) <- paste("Level", 1L:nrow(labels.mat))
    labels <- c("Level 0" = "Total", apply(label.mat, 1, unique))
  }

  # Obtain other information
  names(nodes) <- paste("Level", 0L:(length(nodes) - 1L))

  output <- structure(list(bts = y, nodes = nodes, labels = labels), 
                      class = "hts")
  return(output)
}


# A function to convert the nodes list to gmatrix
GmatrixH <- function(xlist) {
  num.bts <- sum(xlist[[length(xlist)]])
  # Create an empty matrix to contain the gmatrix
  gmat <- matrix(, nrow = length(xlist), ncol = num.bts)
  # Insert the bottom level
  gmat[nrow(gmat), ] <- seq(1L, num.bts)
  # Insert the middle levels in the reverse order
  if (length(xlist) > 1L) {
    repcount <- xlist[[length(xlist)]]
    for (i in (length(xlist) - 1L):1L) {
      gmat[i, ] <- rep(1L:length(xlist[[i + 1]]), repcount)
      repcount <- tapply(repcount, rep(1L:length(xlist[[i]]), xlist[[i]]), sum)
    }
  }
  # Insert the top level
  gmat <- rbind(rep(1L, num.bts), gmat)

  dimnames(gmat) <- list(paste("Level", 0L:(nrow(gmat) - 1L)), colnames(xlist))
  class(gmat) <- "gmatrix"
  return(gmat)
}


# A function to return the NO. of nodes at each level
Mnodes <- function(xlist) {
  m <- c(unlist(lapply(xlist, length)), sum(xlist[[length(xlist)]]))
  return(m)
}


# A function to set the default hierarchical names
HierName <- function(xlist) {
  gmat <- GmatrixH(xlist)  # Based on gmatrix
  # A matrix to store letters
  names.mat <- gmat[-1L, ]
  if (!is.null(nrow(names.mat))) {
    for (i in nrow(names.mat):1L) {
      for (k in 1L:length(xlist[[i]])) {
        end <- cumsum(xlist[[i]])[k]
        start <- end - xlist[[i]][k] + 1
        index <- which(names.mat[i, ] %in% seq(start, end))
        letter <- length(unique(names.mat[i, index]))
        times <- as.data.frame(table(names.mat[i, index]))
        names.mat[i, index] <- rep(LETTERS[1:letter], times[, 2L])
      }
    }
    j <- 2L
    while (j <= nrow(names.mat)) {
      # Overwrite names.mat
      names.mat[j, ] <- paste0(names.mat[j - 1, ], names.mat[j, ])
      j <- j + 1L
    }
    # Drop off the duplicated names
    names.list <- c("Level 0" = "Total", apply(names.mat, 1, unique))
  } else {
    names.list <- list("Level 0" = "Total")
  }
  return(names.list)
}


# A function to check whether it's the "hts" class.
is.hts <- function(xts) {
  is.element("hts", class(xts))
}


# Print "hts" on the screen
print.hts <- function(xts) {
  # ToDo:
  #   1. Add if condition (fcasts) exists
  bts <- xts$bts
  cat("Hierarchical Time Series \n")
  cat(length(Mnodes(xts$nodes)), "Levels \n")
  cat("Number of nodes at each level:", Mnodes(xts$nodes), "\n")
  cat("Total number of series:", sum(Mnodes(xts$nodes)), "\n")
  cat("Number of observations per series:", nrow(bts), "\n")
  cat("Top level series:", "\n")
  
  topts <- ts(rowSums(bts), start = tsp(bts)[1L], frequency = tsp(bts)[3L])
  print(topts)
}

# Functions to contruct hierarchical or grouped time series.

hts <- function(y, nodes) {
  # Construct the hierarchical time series.
  # 
  # Args:
  #   y: The bottom time series assigned by the user. Same lengths and no NA.
  #   nodes: A list contains the number of child nodes for each level except
  #         for the bottom one. 
  #
  # Returns:
  #   A hierarchical time series.
  #
  # ToDo:
  #   1. May handle NA's by forecasting them properly.
  #
  # Error handling:
  if(!is.ts(y)) {
    stop("Agrument y must be a time series data.")
  }
  if(ncol(y) <= 1) {
    stop("Argument y must be a multiviate time series.")
  }
  if(any(is.na(y))) {
    stop("Argument y must not have missing values.")
  }
  if(missing(nodes) || length(as.numeric(nodes)) == 1) {
    nodes <- list(ncol(y))
  } else if(!is.list(nodes)) {
      stop("Argument nodes must be a list.")
  } else {
      for(i in 1:(length(nodes) - 1)) {
        if(sum(nodes[[i]]) != length(nodes[[i + 1]])) {
          error <- paste("The number of nodes for the level", i - 1, "is not 
                         equal to the number of series of level", i)
          stop(error)
        }
      }
      nodes <- nodes
  }
  if(length(nodes[[1]]) != 1) {
    stop("The root node cannot be empty.")
  }
  if(sum(nodes[[length(nodes)]]) != ncol(y)) {
    stop("The number of terminal nodes is not consistent with the number of 
         bottom time series.")
  }

  # Obtain other information
  names(nodes) <- paste("Level", 0:(length(nodes) - 1))

  output <- structure(list(bts = y, nodes = nodes), class = "hts")
  return(output)
}


# A function to convert the nodes list to gmatrix
Gmatrix <- function(xlist) {
  num.bts <- sum(xlist[[length(xlist)]])
  # Create an empty matrix to contain the gmatrix
  gmat <- matrix(, nrow = length(xlist) + 1, ncol = num.bts)
  # Insert the bottom level
  gmat[nrow(gmat), ] <- seq(1, num.bts)
  # Insert the middle levels in the reverse order
  for(i in length(xlist):2) {
    gint <- integer(length = num.bts)
    for(k in 1:length(xlist[[i]])) {
      # Returns the NO. of the unique numbers for each block of the lower level
      num.unique <- cumsum(xlist[[i]])
      if(k == 1) {
        index <- seq(1, num.unique[k])
      } else {
        index <- seq(1, num.unique[k])[-seq(1, num.unique[k - 1])]
      }
      block <- unique(gmat[i + 1, ])[index]
      # Returns the full index for each block
      block.index  <- which(gmat[i + 1, ] %in% block)
      block.length <- length(gmat[i + 1, block.index])
      gint[block.index] <- rep(k, block.length)
    }
    gmat[i, ] <- gint
  }
  # Insert the top level
  gmat[1, ] <- rep(1, num.bts)

  colnames(gmat) <- colnames(xlist)
  rownames(gmat) <- paste("Level", 0:(nrow(gmat) - 1))
  class(gmat) <- "gmatrix"
  return(gmat)
}


# A function to return the NO. of series at each level
Mlevel <- function(xlist) {
  m <- c(unlist(lapply(xlist, length)), sum(xlist[[length(xlist)]]))
  return(m)
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
  cat(length(Mlevel(xts$nodes)), "Levels \n")
  cat("Number of nodes at each level:", Mlevel(xts$nodes), "\n")
  cat("Total number of series:", sum(Mlevel(xts$nodes)), "\n")
  cat("Number of observations per series:", nrow(bts), "\n")
  cat("Top level series:", "\n")
  
  topts <- ts(rowSums(bts), start = tsp(bts)[1], frequency = tsp(bts)[3])
  print(topts)
}

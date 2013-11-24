# Functions to contruct hierarchical or grouped time series.

hts <- function(y, node) {
  # Construct the hierarchical time series.
  # 
  # Args:
  #   y: The bottom time series assigned by the user. Same lengths and no NA.
  #   node: A list contains the number of child nodes for each level except
  #         for the bottom one. 
  #
  # Returns:
  #   A hierarchical time series.
  #
  # Error handling:
  if(!is.ts(y)) {
    stop("Agrument y must be a time series data.")
  }
  if(ncol(y) <= 1) {
    stop("Argument y must be a multiviate time series.")
  }
  if(TRUE %in% is.na(y)) {
    stop("Argument y must not have missing values.")
  }
  if(!is.list(node)) {
    stop("Argument node must be a list.")
  }
  if(length(node[[1]]) != 1) {
    stop("The root node cannot be empty.")
  }
  if(sum(node[[length(node)]]) != ncol(y)) {
    stop("The number of terminal nodes is not consistent with the number of 
         bottome time series.")
  }
  for(i in 1:(length(node) - 1)) {
    if(sum(node[[i]]) != length(node[[i + 1]])) {
      error <- paste("The number of nodes for the level", i - 1, "is not equal
                     to the number of series of level", i)
      stop(error)
    }
  }

  # Obtain the group matrix
  gmatrix <- Gmatrix(node)  # Gmatrix() defined below

  # Obtain other information
  names(node) <- paste("Level", 0:(length(node) - 1))
  # Returns the NO. of series for each level
  m <- apply(gmatrix, 1, function(x) length(unique(x)))

  output <- structure(list(bts = y, node = node, gmatrix = gmatrix, m = m), 
                      class = "hts")
  return(output)
}


# A function to convert the node list to gmatrix
Gmatrix <- function(xlist) {
  num.bts <- sum(xlist[[length(xlist)]])
  # Create an empty matrix to contain the gmatrix
  gmat <- matrix(, nrow = length(xlist) + 1, ncol = num.bts)
  # Insert the bottom level
  gmat[nrow(gmat), ] <- seq(1, num.bts)
  # Insert the middle levels in the reverse order
  for(i in length(xlist):2) {
    gvec <- vector(length = num.bts)
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
      gvec[block.index] <- rep(k, block.length)
    }
    gmat[i, ] <- gvec
  }
  # Insert the top level
  gmat[1, ] <- rep(1, num.bts)

  colnames(gmat) <- colnames(bts)
  rownames(gmat) <- paste("Level", 0:(nrow(gmat) - 1))
  class(gmat) <- "gmatrix"
  return(gmat)
}

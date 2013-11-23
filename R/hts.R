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
  if(is.na(y)) {
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
  for(i in 1:length(node)) {
    if(sum(node[[i]]) != length(node[[i + 1])) {
      error <- paste("The number of nodes for the level", i - 1, "is not equal
                     to the number of series of level", i)
      stop(error)
    }
  }

  # Obtain the group matrix
}


# A function to convert the node list to gmatrix
Gmatrix <- function(xlist) {
  num.bts <- sum(xlist[[length(xlist)]])
  # Create an empty matrix to contain the gmatrix
  gmat <- matrix(, nrow = length(xlist) + 1, ncol = num.bts)
  # Insert the bottom level
  gmat[nrow(gmat), ] <- seq(1:num.bts)
}

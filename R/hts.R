# Functions to contruct hierarchical or grouped time series.

hts <- function(bts, node) {
  # Construct the hierarchical time series.
  # 
  # Args:
  #   bts: The bottom time series assigned by the user. Same lengths and no NA.
  #   node: A list contains the number of children nodes for each level except
  #         for the bottom one. 
  #
  # Returns:
  #   A hierarchical time series.
  #
  # Error handling:
  if(!is.ts(bts)) {
    stop("Agrument bts must be a time series data.")
  }
  if(ncol(bts) <= 1) {
    stop("Argument bts must be a multiviate time series.")
  }
  if(is.na(bts)) {
    stop("Argument bts must not have missing values.")
  }
  if(!is.list(node)) {
    stop("Argument node must be a list.")
  }
  if(length(node[[1]]) != 1) {
    stop("The number of nodes for the level 0 cannot be empty.")
  }
  if(sum(node[[length(node)]]) != ncol(bts)) {
    stop("The number of nodes is not consistent with the number of bottome time
         series.")
  }
  for(i in 1:length(node)) {
    if(sum(node[[i]]) != length(node[[i + 1])) {
      error <- paste("The number of nodes for the level", i - 1, "is not equal
                     to the number of series of level", i)
      stop(error)
    }
  }
}

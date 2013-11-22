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
}

smatrix <- function(xts) {
  # The summing matrix
  #
  # Args:
  #   xts: hts/gts
  #
  # Returns:
  #   S matrix in the sparse form
  library(SparseM)
  if (is.hts(xts)) {
    gmat <- GmatrixH(xts$nodes)
    ntotal <- sum(Mnodes(xts$nodes))
  } else {
    gmat <- xts$groups
    ntotal <- sum(Mlevel(xts$groups))
  }
  nbts <- ncol(xts$bts)
  # The number of non-zero elements
  ra <- as.numeric(rep(1, nbts * nrow(gmat)))

  # Create Iaslot
  Iaslot <- function(x) as.integer(cumsum(c(1L, unlist(apply(gmat, 1, table)))))
  # Create Jaslot
  Jaslot <- function(x) as.integer(rep(gmat[nrow(gmat), ], nrow(gmat)))

  ia <- Iaslot(xts)
  ja <- Jaslot(xts)
  out <- new("matrix.csr", ra = ra, ja = ja, ia = ia, 
             dimension = as.integer(c(ntotal, nbts)))
  return(out)
}

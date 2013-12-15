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
  Iaslot <- function(x) as.integer(cumsum(c(1L, unlist(apply(x, 1, table)))))
  # Create Jaslot
  Jaslot <- function(x) {
    ja <- vector(length = nrow(x), mode = "list")
    uni.num <- apply(x, 1, unique)
    for (i in 1L:length(ja)) {
      ja[[i]] <- integer(length = ncol(x))
      for (k in 1L:length(uni.num[[i]])) {
        ja[[i]][k] <- which(x[i, ] %in% uni.num[[i]][k])
      }
    }
    return(as.integer(unlist(ja)))
  }
  # Jaslot <- function(x) as.integer(rep(x[nrow(x), ], nrow(x)))

  ia <- Iaslot(gmat)
  ja <- Jaslot(gmat)
  out <- new("matrix.csr", ra = ra, ja = ja, ia = ia, 
             dimension = as.integer(c(ntotal, nbts)))
  return(out)
}

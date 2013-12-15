smatrix <- function(xts) {
  # The summing matrix
  #
  # Args:
  #   xts: hts/gts
  #
  # Returns:
  #   S matrix in the sparse mode
  library(SparseM)
  if (is.hts(xts)) {
    gmat <- GmatrixH(xts$nodes)
    ntotal <- sum(Mnodes(xts$nodes))
    Jaslot <- function(x) as.integer(rep(x[nrow(x), ], nrow(x)))
  } else {
    gmat <- xts$groups
    ntotal <- sum(Mlevel(xts$groups))
  }
  nbts <- ncol(xts$bts)
  # The number of non-zero elements
  ra <- as.numeric(rep(1, nbts * nrow(gmat)))

  # Create Iaslot
  Iaslot <- function(xmat) as.integer(cumsum(c(1L, 
                                      unlist(apply(xmat, 1, table)))))
  # Create Jaslot
  Jaslot <- function(xmat) {
    ja <- vector(length = nrow(xmat), mode = "list")
    uni.num <- apply(xmat, 1, unique)
    if (is.matrix(uni.num)) {
      uni.num <- split(uni.num, rep(1L:ncol(uni.sum), each = nrow(uni.num)))
    }
    for (i in 1L:length(ja)) {
      ja[[i]] <- unlist(sapply(uni.num[[i]], 
                        function(x) which(xmat[i, ] %in% x), simplify = FALSE))
    }
    return(as.integer(unlist(ja)))
  }

  ia <- Iaslot(gmat)
  ja <- Jaslot(gmat)
  out <- new("matrix.csr", ra = ra, ja = ja, ia = ia, 
             dimension = as.integer(c(ntotal, nbts)))
  return(out)
}

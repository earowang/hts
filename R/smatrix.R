smatrix <- function(xts) {
  # The summing matrix
  #
  # Args:
  #   xts: hts/gts
  #
  # Returns:
  #   S matrix in the dense mode
  if (!is.gts(xts)) {
    stop("Argument xts must be a gts object")
  }
  return(as.matrix(Smatrix(xts)))
}

Smatrix <- function(xts) {
  # S matrix in the sparse mode
  if (is.hts(xts)) {
    gmat <- GmatrixH(xts$nodes)
    mnodes <- Mnodes(xts$nodes)
    ntotal <- sum(mnodes)
    nbts <- mnodes[length(mnodes)]
  } else {
    gmat <- xts$groups
    mlevel <- Mlevel(xts$groups)
    ntotal <- sum(mlevel)
    nbts <- mlevel[length(mlevel)]
  }
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
      uni.num <- split(uni.num, rep(1L:ncol(uni.num), each = nrow(uni.num)))
    }
    for (i in 1L:length(ja)) {
      ja[[i]] <- unlist(sapply(uni.num[[i]], 
                        function(x) which(xmat[i, ] %in% x), simplify = FALSE))
    }
    return(as.integer(unlist(ja)))
  }

  ia <- Iaslot(gmat)
  ja <- Jaslot(gmat)
  return(new("matrix.csr", ra = ra, ja = ja, ia = ia, 
             dimension = as.integer(c(ntotal, nbts))))
}

smatrix <- function(xts) {
  # The summing matrix
  #
  # Args:
  #   xts: hts/gts
  #
  # Returns:
  #   S matrix in the dense mode
  return(as.matrix(Smatrix(xts)))
}

Smatrix <- function(xts) {
  # S matrix in the sparse mode
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


# A function to convert sparse matrix to group matrix for gts
S2g <- function(smat) {
  smat <- as.matrix(smat)  # dense matrix
  k <- 1L  # k = group within level
  i <- 0L  # i = level
  g <- NULL  # group matrix
  top <- rep(1L, ncol(smat))
  for (j in 1L:nrow(smat)) {
    if (sum(abs(top - 1)) <= 0L) {
      g <- rbind(g, smat[j, ])
      top <- smat[j, ]
      k <- 2
      i <- i + 1L
    } else {
      g[i, ] <- g[i, ] + smat[j, ] * k
      k <- k + 1L
      top <- top + smat[j, ]
    }
  }
  g <- g[-c(1L, nrow(g)), ]  # remove the first and last row
  return(g)
}

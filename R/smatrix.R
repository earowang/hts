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
  if (is.hts(xts)) {
    gmat <- GmatrixH(xts$nodes)
  } else {
    gmat <- xts$groups
  }
  return(as.matrix(SmatrixM(gmat)))
}

# This function returns a sparse matrix supported by Matrix pkg
SmatrixM <- function(gmat) { 
  # Sparse matrices stored in coordinate format
  # gmatrix contains all the information to generate smatrix
  num.bts <- ncol(gmat)
  sparse.S <- apply(gmat, 1L, function(x) {
    ia <- as.integer(x)
    ra <- as.integer(rep(1L, num.bts))
    ja <- as.integer(1L:num.bts)
    s <- sparseMatrix(i = ia, j = ja, x = ra)
  })
  sparse <- do.call("rBind", sparse.S)
  return(sparse)
}

# This function returns a sparse matrix supported by SparseM pkg
Smatrix <- function(gmat) {
  # Sparse matrices stored in coordinate format
  # gmatrix contains all the information to generate smatrix
  num.bts <- ncol(gmat)
  sparse.S <- apply(gmat, 1L, function(x) {
    ia <- as.integer(x)
    uniq.g <- unique(ia)
    ra <- as.integer(rep(1L, num.bts))
    ja <- as.integer(1L:num.bts)
    s <- as.matrix.csr(new("matrix.coo", ra = ra, ja = ja, ia = ia,
           dimension = as.integer(c(length(uniq.g), num.bts))))
  })
  sparse <- do.call("rbind", sparse.S)
  return(sparse)
}

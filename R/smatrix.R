#' Summing matrix for hierarchical or grouped time series
#' 
#' This function returns the summing matrix for a hierarchical or grouped time
#' series, as defined in Hyndman et al. (2011).
#' 
#' 
#' @param xts Hierarchical or grouped time series of class \code{gts}.
#' @return A numerical matrix.
#' @author Earo Wang
#' @seealso \code{\link[hts]{hts}}, \code{\link[hts]{gts}},
#' \code{\link[hts]{combinef}}
#' @references R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang
#' (2011) Optimal combination forecasts for hierarchical time series.
#' \emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
#' \url{http://robjhyndman.com/papers/hierarchical/}
#' @keywords ts
#' @examples
#' 
#' smatrix(htseg1)
#' 
#' @export smatrix
smatrix <- function(xts) {
  # The summing matrix
  #
  # Args:
  #   xts: hts/gts
  #
  # Returns:
  #   S matrix in the dense mode
  if (!is.gts(xts)) {
    stop("Argument xts must be a gts object", call. = FALSE)
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
  sparse <- do.call("rbind", sparse.S)
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

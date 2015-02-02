# 3 algorithms for forecasting reconciliation through trace minimization
# Only used for BLUF
# Author: Shanika Wickramasuriya
# Paper: Forecasting hierarchical and grouped time series through trace 
#        minimization
# All these functions return a reverse reconciled matrix with all ts.

# LU factorization (Matrix pkg)
LU <- function(fcasts, S, weights) {
  nts <- nrow(S)
  nbts <- ncol(S)
  nagg <- nts - nbts
  seqagg <- 1L:nagg
  utmat <- cbind2(sparseMatrix(i = seqagg, j = seqagg, x = 1),
                  -1 * S[1L:nagg, ])
  jmat <- sparseMatrix(i = 1L:nbts, j = (nagg + 1L):nts, x = rep(1L, nbts),
                       dims = c(nbts, nts))
  rhs.l <- as(utmat %*% fcasts, "CsparseMatrix")
  if (is.null(weights)) {
    lhs.l <- utmat %*% t(utmat)
    lhs.l <- (t(lhs.l) + lhs.l)/2
    lin.sol <- solve(lhs.l, rhs.l)
    p1 <- jmat %*% fcasts - (jmat %*% t(utmat) %*% lin.sol)
  } else {
    lhs.l <- utmat %*% weights %*% t(utmat)
    lhs.l <- (t(lhs.l) + lhs.l)/2
    lin.sol <- solve(lhs.l, rhs.l)
    p1 <- jmat %*% fcasts - (jmat %*% weights %*% t(utmat) %*% lin.sol)
  }
  comb <- as.matrix(S %*% p1)
  return(comb)
}

# Conjugate Gradient (Matrix and RcppEigen pkgs)
CG <- function(fcasts, S, weights) {
  nts <- nrow(S)
  nbts <- ncol(S)
  nagg <- nts - nbts
  seqagg <- 1L:nagg
  utmat <- cbind2(sparseMatrix(i = seqagg, j = seqagg, x = 1),
                  -1 * S[1L:nagg, ])
  jmat <- sparseMatrix(i = 1L:nbts, j = (nagg + 1L):nts, x = rep(1L, nbts),
                       dims = c(nbts, nts))
  rhs.l <- as.matrix(utmat %*% fcasts)
  if (is.null(weights)) {
    lhs.l <- utmat %*% t(utmat)
    lin.sol <- as.matrix(cgm_c(lhs.l, rhs.l)) # cgm_c is a C++ function
    p1 <- jmat %*% fcasts - (jmat %*% t(utmat) %*% lin.sol)
  } else {
    lhs.l <- utmat %*% weights %*% t(utmat)
    lin.sol <- as.matrix(cgm_c(lhs.l, rhs.l))
    p1 <- jmat %*% fcasts - (jmat %*% weights %*% t(utmat) %*% lin.sol)
  }
  comb <- as.matrix(S %*% p1)
  return(comb)
}

# Cholesky factorization
CHOL <- function(fcasts, S, weights) {
  nts <- nrow(S)
  nbts <- ncol(S)
  nagg <- nts - nbts
  seqagg <- 1L:nagg
  utmat <- cbind(as(nagg, "matrix.diag.csr"), -1 * S[1L:nagg, ])
  jmat <- new("matrix.csr", ra = rep(1L, nbts), ja = seq((nagg + 1L), nts),
              ia = 1L:(nbts + 1L), dimension = as.integer(c(nbts, nts)))
  rhs.l <- utmat %*% fcasts
  if (is.null(weights)) {
    lhs.l <- utmat %*% t(utmat)
    lhs.l <- (t(lhs.l) + lhs.l)/2
    lin.sol <- backsolve(chol(lhs.l), rhs.l)
    p1 <- jmat %*% fcasts - (jmat %*% t(utmat) %*% lin.sol)
  } else {
    lhs.l <- utmat %*% weights %*% t(utmat)
    lhs.l <- (t(lhs.l) + lhs.l)/2
    lin.sol <- backsolve(chol(lhs.l), rhs.l)
    p1 <- jmat %*% fcasts - (jmat %*% weights %*% t(utmat) %*% lin.sol)
  }
  comb <- as.matrix(S %*% p1)
  return(comb)
}

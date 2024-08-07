# 3 algorithms for forecasting reconciliation through trace minimization
# Only used for BLUF
# Author: Shanika Wickramasuriya
# Paper: Forecasting hierarchical and grouped time series through trace
#        minimization
# All these functions return a reverse reconciled matrix with all ts.

#LU decomposition is fast but sometimes instable. Use QR decomposition if LU decomposition fails
solveLUQR <- function(lhs.l, rhs.l) {
  tryCatch(solve(lhs.l, rhs.l), error=function(cond){

        #browser()
        warning("An error in LU decomposition occurred, the message was the following:\n",
            cond$message, "\n Trying QR decomposition instead...")
        solve(qr(lhs.l), rhs.l)
      })
}

# LU factorization (Matrix pkg)
LU <- function(fcasts, S, weights, allow.changes = FALSE) {
  nts <- nrow(S)
  nbts <- ncol(S)
  nagg <- nts - nbts
  seqagg <- 1L:nagg

  if (!allow.changes) {
    utmat <- cbind2(sparseMatrix(i = seqagg, j = seqagg, x = 1),
                    -1 * S[1L:nagg, , drop = TRUE])
  } else {
    # Identifying rows with one 1 element to make the Identity matrix in S
    indx <- rowSums(S)
    idx <- tail(which(indx == 1L), nbts)

    # Permulation vector to rearrange rows of S, rows/col of W and forecasts
    pvec <- c(setdiff(1:nts, idx) , idx)
    S2 <- S[pvec, ]
    weights <- weights[pvec, pvec]
    fcasts <- fcasts[pvec, ]
    utmat <- cbind2(sparseMatrix(i = seqagg, j = seqagg, x = 1),
                    -1 * S2[1L:nagg, , drop = TRUE])
  }
  jmat <- sparseMatrix(i = 1L:nbts, j = (nagg + 1L):nts, x = rep(1L, nbts),
                       dims = c(nbts, nts))
  rhs.l <-  methods::as(utmat %*% fcasts, "CsparseMatrix")
  if (is.null(weights)) {
    lhs.l <- utmat %*% t(utmat)
    lhs.l <- (t(lhs.l) + lhs.l)/2
    lin.sol <- solveLUQR(lhs.l, rhs.l)
    p1 <- jmat %*% fcasts - (jmat %*% t(utmat) %*% lin.sol)
  } else {
    lhs.l <- utmat %*% weights %*% t(utmat)
    lhs.l <- (t(lhs.l) + lhs.l)/2
    lin.sol <- solveLUQR(lhs.l, rhs.l)
    p1 <- jmat %*% fcasts - (jmat %*% weights %*% t(utmat) %*% lin.sol)
  }
  if (!allow.changes) {
    comb <- as.matrix(S %*% p1)
  } else {
    comb <- numeric()
    comb[pvec] <- as.matrix(S2 %*% p1)
  }
  return(comb)
}

# Conjugate Gradient (Matrix and RcppEigen pkgs)
CG <- function(fcasts, S, weights, allow.changes = FALSE) {
  nts <- nrow(S)
  nbts <- ncol(S)
  nagg <- nts - nbts
  seqagg <- 1L:nagg
  if (!allow.changes) {
    utmat <- cbind2(Matrix::sparseMatrix(i = seqagg, j = seqagg, x = 1),
                    -1 * S[1L:nagg, ])
  } else {
    # Identifying rows with one 1 element to make the Identity matrix in S
    indx <- rowSums(S)
    idx <- tail(which(indx == 1L), nbts)

    # Permulation vector to rearrange rows of S, rows/col of W and forecasts
    pvec <- c(setdiff(1:nts, idx) , idx)
    S2 <- S[pvec, ]
    weights <- weights[pvec, pvec]
    fcasts <- fcasts[pvec, ]
    utmat <- cbind2(Matrix::sparseMatrix(i = seqagg, j = seqagg, x = 1),
                    -1 * S2[1L:nagg, ])
  }
  jmat <- Matrix::sparseMatrix(i = 1L:nbts, j = (nagg + 1L):nts, x = rep(1L, nbts),
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
  if (!allow.changes) {
    comb <- as.matrix(S %*% p1)
  } else {
    comb <- numeric()
    comb[pvec] <- as.matrix(S2 %*% p1)
  }
  return(comb)
}

# Cholesky factorization
CHOL <- function(fcasts, S, weights, allow.changes = FALSE) {
  fcasts <- t(stats::na.omit(t(fcasts)))
  nts <- nrow(S)
  nbts <- ncol(S)
  nagg <- nts - nbts
  seqagg <- 1L:nagg
  if (!allow.changes) {
    utmat <- cbind(methods::as(nagg, "matrix.diag.csr"), -1 * S[1L:nagg, ])
  } else {
    # Identifying rows with one 1 element to make the Identity matrix in S
    Sm <- as(S, "dgCMatrix")
    indx <- rowSums(Sm)
    idx <- tail(which(indx == 1L), nbts)

    # Permulation vector to rearrange rows of S, rows/col of W and forecasts
    pvec <- c(setdiff(1:nts, idx) , idx)
    S2 <- S[pvec, ]
    weights <- weights[pvec, pvec]
    fcasts <- fcasts[pvec, ]
    utmat <- cbind(methods::as(nagg, "matrix.diag.csr"), -1 * S2[1L:nagg, ])
  }
  jmat <- methods::new("matrix.csr", ra = rep(1L, nbts), ja = seq((nagg + 1L), nts),
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
  if (!allow.changes) {
    comb <- as.matrix(S %*% p1)
  } else {
    comb <- numeric()
    comb[pvec] <- as.matrix(S2 %*% p1)
  }
  return(comb)
}

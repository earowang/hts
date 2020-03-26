# helper function to block principal pivoting algorithm
# can only be used with OLS, WLS (any positive weights)
# Author: Shanika Wickramasuriya
# Paper: Optimal non-negative forecast reconciliation

# Arguments
# fcasts: a vector of h-steps-ahead forecasts for all levels of the hierarchical time series. 
# smat: updated original s-matrix (based on the active set constraints)
# weights: updated weights to be used in OLS or WLS
# alg: algorithm such as "lu", "chol" or "cg"

combinefm <- function(fcasts, smat, weights, alg) 
{
  totalts <- nrow(smat)
  if (!is.matrix(fcasts)) {
    fcasts <- t(fcasts)
  }
  if (ncol(fcasts) != totalts) {
    stop("Argument fcasts requires all the forecasts.")
  }
  
  fcasts <- t(fcasts)
  if (alg == "chol") {
    if (!is.null(weights)) {
      weights <- methods::as(1/weights, "matrix.diag.csr")
    }
    allf <- CHOL(fcasts = fcasts, S = smat, weights = weights, allow.changes = TRUE)
  } else {
    if (!is.null(weights)) {
      seqts <- 1:totalts
      weights <- sparseMatrix(i = seqts, j = seqts, x = 1/weights)
    }
    if (alg == "lu") {
      allf <- LU(fcasts = fcasts, S = smat, weights = weights, allow.changes = TRUE)
    } else if (alg == "cg") {
      allf <- CG(fcasts = fcasts, S = smat, weights = weights, allow.changes = TRUE)
    }
  }
  return(allf)
}



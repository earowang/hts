combinefm <- function(fcasts, nodes, groups, smat, weights, alg) 
{
  totalts <- nrow(smat)
  if (!is.matrix(fcasts)) {
    fcasts <- t(fcasts)
  }
  if (ncol(fcasts) != totalts) {
    stop("Argument fcasts requires all the forecasts.")
  }
  
  if (missing(groups)) { # hts class
    # Other algorithms return all time series
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
  } else {# gts class
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
  }
  return(allf)
}



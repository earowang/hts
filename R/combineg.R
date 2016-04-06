SLM <- function(fcasts, S, weights = NULL) {
  class(fcasts) <- stats::tsp(fcasts) <- NULL
  fcasts <- t(stats::na.omit(t(fcasts))) # In case of "NA"
  if (is.null(weights)) {
    coef <- SparseM::slm.fit(S, fcasts)$coefficients
  } else {
    coef <- SparseM::slm.wfit(S, fcasts, weights = weights)$coefficients
  }
  fitted.v <- as.matrix(S %*% coef)
  return(fitted.v)
}

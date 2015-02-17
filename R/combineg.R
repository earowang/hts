SLM <- function(fcasts, S, weights = NULL) {
  class(fcasts) <- tsp(fcasts) <- NULL
  fcasts <- na.omit(fcasts) # In case of "NA"
  if (is.null(weights)) {
    coef <- slm.fit(S, fcasts)$coefficients
  } else {
    coef <- slm.wfit(S, fcasts, weights = weights)$coefficients
  }
  fitted.v <- as.matrix(S %*% coef)
  return(fitted.v)
}

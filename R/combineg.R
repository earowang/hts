CombineG <- function(fcasts, S, weights = NULL) {
  class(fcasts) <- tsp(fcasts) <- NULL
  fcasts <- na.omit(fcasts)  # Used for "rw" fmethod
  if (is.null(weights)) {
    coef <- slm.fit(S, t(fcasts))$coefficients
  } else {
    coef <- slm.wfit(S, t(fcasts), weights = weights)$coefficients
  }
  bottom <- nrow(S) - (ncol(S):1L) + 1L
  fitted.v <- as.matrix(S %*% coef)
  if (is.vector(fitted.v)) {  # h = 1
    out <- t(fitted.v[bottom])
  } else {
    out <- t(fitted.v[bottom, ])  # Only return bottom level
  }
  return(out)
}

CombineG <- function(fcasts, S, weights = NULL) {
  class(fcasts) <- tsp(fcasts) <- NULL
  if (is.null(weights)) {
    coef <- slm.fit(S, t(fcasts), na.action = na.omit)$coefficients
  } else {
    coef <- slm.wfit(S, t(fcasts), weights = weights, 
                     na.action = na.omit)$coefficients
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

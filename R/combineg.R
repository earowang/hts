CombineG <- function(fcasts, S, weights = NULL) {
  class(fcasts) <- tsp(fcasts) <- NULL
  if (is.null(weights)) {
    fit <- slm.fit(S, t(fcasts))
  } else {
    fit <- slm.wfit(S, t(fcasts), weights = weights)
  }
  bottom <- nrow(S) - (ncol(S):1L) + 1L
  fitted.v <- as.matrix(S %*% fit$coefficients)
  if (is.vector(fitted.v)) {  # h = 1
    out <- t(fitted.v[bottom])
  } else {
    out <- t(fitted.v[bottom, ])  # Only return bottom level
  }
  return(out)
}

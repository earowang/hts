CombineG <- function(fcasts, S, weights = NULL) {
  class(fcasts) <- tsp(fcasts) <- NULL
  if (is.null(weights)) {
    fit <- slm.fit(S, t(fcasts))
  } else {
    fit <- slm.wfit(S, t(fcasts), weights = weights)
  }
  bottom <- nrow(S) - (ncol(S):1L) + 1L
  out <- t(fitted(fit)[bottom, ])  # Only return bottom level
  return(out)
}

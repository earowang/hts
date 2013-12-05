forecast.gts <- function(object, h = ifelse(frequency(object) > 1L, 
                         2L*frequency(object), 10L), method = c("bu"),
                         fmethod = c("ets", "arima", "rw"), ...) {
  # Forecast hts or gts objects
  #
  # Args:
  #   object*: Only hts/gts can be passed onto this function.
  #   h: h-step forecasts.
  #   method: Aggregated approaches.
  #   fmethod: Forecast methods.
  #
  # Return:
  #   Point forecasts with other info chosen by the user.
  #
  require(forecast)
  method <- match.arg(method)
  fmethod <- match.arg(fmethod)
  # Error Handling:
  if (!is.gts(object)) {
    stop("Argument object must be either a hts or gts object.")
  }
  if (h < 1L) {
    stop("Argument h must be positive.")
  }

  # Bottom-up approach
  if (method == "bu") {
    ally <- object$bts  # Only grab the bts
    if (is.hts(object)) {
      gmat <- GmatrixH(object$nodes)
    } else {
      gmat <- object$groups
    }
  }
  
  if (fmethod == "ets") {
    # Fit a model
    fit <- lapply(ally, function(x) ets(x, ...))
    # Generate point forecasts at the bottom level
    fcasts <- sapply(fit, function(x) forecast(x, h = h)$mean)
  } else if (fmethod == "arima") {
    fit <- lapply(ally, function(x) auto.arima(x, ...))
    fcasts <- sapply(fit, function(x) forecast(x, h = h)$mean)
  } else {
    fcasts <- sapply(ally, function(x) rwf(x, h = h)$mean)
  }

  if (method == "bu") {
    allfcasts <- BottomUp(fcasts, gmat)
  }

  tsp.ally <- tsp(ally)
  allfcasts <- ts(allfcasts, start = tsp.ally[2L] + 1L/tsp.ally[3L], 
                  frequency = tsp.ally[3L])
  return(allfcasts)
}

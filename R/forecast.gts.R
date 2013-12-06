forecast.gts <- function(object, h = ifelse(frequency(object) > 1L,
                         2L*frequency(object), 10L), 
                         method = c("bu"),
                         fmethod = c("ets", "arima", "rw"), 
                         keep = c(NULL, "fitted", "residuals"),
                         positive = FALSE, ...) {
  # Forecast hts or gts objects
  #
  # Args:
  #   object*: Only hts/gts can be passed onto this function.
  #   h: h-step forecasts.
  #   method: Aggregated approaches.
  #   fmethod: Forecast methods.
  #   keep: Users specify what they'd like to keep at the bottom level.
  #   positive: Put restrictions on positive forecasts.
  #
  # Return:
  #   Point forecasts with other info chosen by the user.
  #
  require(forecast)
  method <- match.arg(method)
  fmethod <- match.arg(fmethod)
  keep <- match.arg(keep)
  # Error Handling:
  if (!is.gts(object)) {
    stop("Argument object must be either a hts or gts object.")
  }
  if (h < 1L) {
    stop("Argument h must be positive.")
  }

  # Set up lambda for arg "positive"
  if (positive) {
    lambda <- 0
  } else {
    lambda <- NULL
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
    fit <- lapply(ally, function(x) ets(x, lambda = lambda, ...))
    if (keep == "fitted") {
      fits <- sapply(ally, function(x) fitted(ets(x, lambda = lambda, ...)))
    } else if (keep == "residuals") {
      res <- sapply(ally, function(x) residuals(ets(x, lambda = lambda, ...)))
    }
    # Generate point forecasts at for all level
    pfcasts <- sapply(fit, function(x) forecast(x, h = h)$mean)
  } else if (fmethod == "arima") {
    fit <- lapply(ally, function(x) auto.arima(x, lambda = lambda, ...))
    if (keep == "fitted") {
      fits <- sapply(ally, function(x) fitted(auto.arima(x, lambda = lambda, 
                     ...)))
    } else if (keep == "residuals") {
      res <- sapply(ally, function(x) residuals(auto.arima(x, lambda = lambda, 
                    ...)))
    }
    pfcasts <- sapply(fit, function(x) forecast(x, h = h)$mean)
  } else {
    if (keep == "fitted") {
      fits <- sapply(ally, function(x) fitted(rwf(x, lambda = lambda, ...)))
    } else if (keep == "residuals") {
      res <- sapply(ally, function(x) residuals(rwf(x, lambda = lambda, ...)))
    }
    pfcasts <- sapply(ally, function(x) rwf(x, h = h, lambda = lambda)$mean)
  }

  if (method == "bu") {
    allfcasts <- BottomUp(pfcasts, gmat)
    if (exists("fits")) {
      bfits <- fits
    } else if (exists("res")) {
      bres <- res
    }
  }

  tsp.ally <- tsp(ally)
  allfcasts <- ts(allfcasts, start = tsp.ally[2L] + 1L/tsp.ally[3L], 
                  frequency = tsp.ally[3L])
  return(list(allfcasts, 
              fitted = if (exists("bfits")) bfits, 
              residuals = if (exists("bres")) bres))
}

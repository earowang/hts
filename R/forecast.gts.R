forecast.gts <- function(object, h = ifelse(frequency(object) > 1L,
                         2L*frequency(object), 10L), 
                         method = c("bu"),
                         fmethod = c("ets", "arima", "rw"), 
                         keep = c(NaN, "fitted", "residuals"),
                         positive = FALSE, lambda = NULL, 
                         xreg = NULL, newxreg = NULL, ...) {
  # Forecast hts or gts objects
  #
  # Args:
  #   object*: Only hts/gts can be passed onto this function.
  #   h: h-step forecasts.
  #   method: Aggregated approaches.
  #   fmethod: Forecast methods.
  #   keep: Users specify what they'd like to keep at the bottom level.
  #   positive & lambda: Use Box-Cox transformation.
  #
  # Return:
  #   Point forecasts with other info chosen by the user.
  #
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

  # Set up lambda for arg "positive" when lambda is missing
  if (is.null(lambda)) {
    if (positive) {
      lambda <- 0
    } else {
      lambda <- NULL
    }
  }

  # Bottom-up approach
  if (method == "bu") {
    y <- object$bts  # Only grab the bts
  }

  # Pre-allocate memory
  model <- vector(length = ncol(y), mode = "list")
  if (keep == "fitted") {
    fits <- matrix(, nrow = nrow(y), ncol = ncol(y))
  } else if (keep == "residuals") {
    resid <- matrix(, nrow = nrow(y), ncol = ncol(y))
  }

  if (fmethod == "ets") {
    # Fit a model
    for (i in 1L:ncol(y)) {
      model[[i]] <- ets(y[, i], lambda = lambda, ...)
      if (keep == "fitted") {
        fits[, i] <- fitted(model[[i]])  # Grab fitted values
      } else if (keep == "residuals") {
        resid[, i] <- residuals(model[[i]])  # Grab residuals
      }
    }
    # Generate point forecasts at for all level
    pfcasts <- sapply(model, function(x) forecast(x, h = h, PI = FALSE)$mean)
  } else if (fmethod == "arima") {
    for (i in 1L:ncol(y)) {
      model[[i]] <- auto.arima(y[, i], lambda = lambda, xreg = xreg, ...)
      if (keep == "fitted") {
        fits[, i] <- fitted(model[[i]])  # Grab fitted values
      } else if (keep == "residuals") {
        resid[, i] <- residuals(model[[i]])  # Grab residuals
      }
    }
    pfcasts <- sapply(model, function(x) forecast(x, h = h, xreg = newxreg, 
                      PI = FALSE)$mean)
  } else {
    for (i in 1L:ncol(y)) {
      model[[i]] <- rwf(y[, i], lambda = lambda, ...)
      if (keep == "fitted") {
        fits[, i] <- fitted(model[[i]])  # Grab fitted values
      } else if (keep == "residuals") {
        resid[, i] <- residuals(model[[i]])  # Grab residuals
      }
    }
    pfcasts <- sapply(y, function(x) rwf(x, h = h, lambda = lambda)$mean)
  }

  tsp.y <- tsp(y)
  if (method == "bu") {
    bfcasts <- ts(pfcasts, start = tsp.y[2L] + 1L/tsp.y[3L], 
                  frequency = tsp.y[3L])
    if (exists("fits")) {
      bfits <- ts(fits, start = tsp.y[2L], frequency = tsp.y[3L])
    } else if (exists("resid")) {
      bresid <- ts(resid, start = tsp.y[2L], frequency = tsp.y[3L])
    }
  }

  return(list(f = bfcasts, 
              fitted = if (exists("bfits")) bfits, 
              residuals = if (exists("bresid")) bresid))
}

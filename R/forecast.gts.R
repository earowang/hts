forecast.gts <- function(object, h = ifelse(frequency(object) > 1L,
                         2L*frequency(object), 10L), 
                         method = c("bu"),
                         fmethod = c("ets", "arima", "rw"), 
                         keep.fitted = FALSE, keep.resid = FALSE,
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
  if (keep.fitted) {
    fits <- matrix(, nrow = nrow(y), ncol = ncol(y))
  } 
  if (keep.resid) {
    resid <- matrix(, nrow = nrow(y), ncol = ncol(y))
  }

  if (fmethod == "ets") {
    # Fit a model
    for (i in 1L:ncol(y)) {
      model[[i]] <- ets(y[, i], lambda = lambda, ...)
      if (keep.fitted) {
        fits[, i] <- fitted(model[[i]])  # Grab fitted values
      } 
      if (keep.resid) {
        resid[, i] <- residuals(model[[i]])  # Grab residuals
      }
    }
    # Generate point forecasts at for all level
    pfcasts <- sapply(model, function(x) forecast(x, h = h, PI = FALSE)$mean)
  } else if (fmethod == "arima") {
    for (i in 1L:ncol(y)) {
      model[[i]] <- auto.arima(y[, i], lambda = lambda, xreg = xreg, ...)
      if (keep.fitted) {
        fits[, i] <- fitted(model[[i]])  # Grab fitted values
      } 
      if (keep.resid) {
        resid[, i] <- residuals(model[[i]])  # Grab residuals
      }
    }
    pfcasts <- sapply(model, function(x) forecast(x, h = h, xreg = newxreg, 
                      PI = FALSE)$mean)
  } else {
    for (i in 1L:ncol(y)) {
      model[[i]] <- rwf(y[, i], lambda = lambda, ...)
      if (keep.fitted) {
        fits[, i] <- fitted(model[[i]])  # Grab fitted values
      } 
      if (keep.resid) {
        resid[, i] <- residuals(model[[i]])  # Grab residuals
      }
    }
    pfcasts <- sapply(y, function(x) rwf(x, h = h, lambda = lambda)$mean)
  }

  # Set up basic info
  tsp.y <- tsp(y)

  if (method == "bu") {
    bfcasts <- ts(pfcasts, start = tsp.y[2L] + 1L/tsp.y[3L], 
                  frequency = tsp.y[3L])
  }
  colnames(bfcasts) <- colnames(object$bts)
  if (keep.fitted) {
    bfits <- ts(fits, start = tsp.y[2L], frequency = tsp.y[3L])
    colnames(bfits) <- colnames(object$bts)
  } 
  if (keep.resid) {
    bresid <- ts(resid, start = tsp.y[2L], frequency = tsp.y[3L])
    colnames(bresid) <- colnames(object$bts)
  }

  return(structure(
           list(bts = bfcasts, 
           histy = object$bts,
           fitted = if (exists("bfits")) bfits, 
           residuals = if (exists("bresid")) bresid,
           nodes = if (is.hts(object)) object$nodes, 
           groups = if (is.gts(object)) object$groups,
           labels = object$labels,
           method = method,
           fmethod = fmethod
           ), class = if (is.hts(object)) c("gts", "hts") else "gts"))
}

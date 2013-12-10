forecast.gts <- function(object, h = ifelse(frequency(object) > 1L,
                         2L*frequency(object), 10L), 
                         method = c("comb", "bu"),
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
      if (any(object < 0L)) {
        stop("Some data are negative.")
      }
      lambda <- 0
    } else {
      lambda <- NULL
    }
  }

  # Set up forecast methods
  if (method == "comb") { # Combination
    y <- aggts(object)  # Grab all ts
  } else if (method == "bu") {  # Bottom-up approach
    y <- object$bts  # Only grab the bts
  }

  # Pre-allocate memory
  model <- vector(length = ncol(y), mode = "list")
  if (keep.fitted || keep.resid) {
    fits <- resid <- matrix(, nrow = nrow(y), ncol = ncol(y))
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
  bnames <- colnames(object$bts)

  if (method == "comb") {
    # bfcasts <- combinefw(pfcasts, object$nodes)
    bfcasts <- combinef(pfcasts, object$nodes)
    if (keep.fitted) {
      fits <- combinef(fits, object$nodes)
    }
    if (keep.resid) {
      resid <- combinef(resid, object$nodes)
    }
  } else if (method == "bu") {
    bfcasts <- pfcasts
  }

  if (is.vector(bfcasts)) {  # if h = 1, sapply returns a vector
    bfcasts <- t(bfcasts)
  }

  bfcasts <- ts(bfcasts, start = tsp.y[2L] + 1L/tsp.y[3L], 
                frequency = tsp.y[3L])
  colnames(bfcasts) <- bnames
  if (keep.fitted) {
    bfits <- ts(fits, start = tsp.y[2L], frequency = tsp.y[3L])
    colnames(bfits) <- bnames
  } 
  if (keep.resid) {
    bresid <- ts(resid, start = tsp.y[2L], frequency = tsp.y[3L])
    colnames(bresid) <- bnames
  }

  # Output
  out <- list(bts = bfcasts, histy = object$bts, labels = object$labels,
              method = method, fmethod = fmethod)
  if (exists("bfits")) {
    out <- c(out, fitted = list(bfits))
  }
  if (exists("bresid")) {
    out <- c(out, residuals = list(bresid))
  }
  if (is.hts(object)) {
    out <- c(out, nodes = list(object$nodes))
  }
  if (is.gts(object) && !is.hts(object)) {
    out <- c(out, groups = list(object$groups))
  }

  return(structure(out, class = if (is.hts(object)) c("gts", "hts") else "gts"))
}

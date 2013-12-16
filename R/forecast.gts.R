forecast.gts <- function(object, h = ifelse(frequency(object) > 1L,
                         2L*frequency(object), 10L), 
                         method = c("comb", "bu", "tdgsa", "tdgsf"),
                         fmethod = c("ets", "arima", "rw"), 
                         keep.fitted = FALSE, keep.resid = FALSE,
                         positive = FALSE, lambda = NULL, 
                         weights = c("none", "sd", "nseries"),
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
  method <- match.arg(method)
  fmethod <- match.arg(fmethod)
  weights <- match.arg(weights)
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

  if (weights == "sd") {
    keep.resid <- TRUE
  }

  # Set up forecast methods
  if (method == "comb") { # Combination
    y <- aggts(object)  # Grab all ts
  } else if (method == "bu") {  # Bottom-up approach
    y <- object$bts  # Only grab the bts
  } else if (substr(method, 1L, 2L) == "td") {
    y <- aggts(object, levels = 0)  # Grab the top ts
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
  # class used for combinef to detect hts/gts
  class(pfcasts) <- class(object)
  if (keep.fitted) {
    class(fits) <- class(object)
  }
  if (keep.resid) {
    class(resid) <- class(object)
  }

  if (method == "comb") {
    if (is.hts(object)) {
      gr <- object$nodes
    } else {
      gr <- smatrix(object)
    }
  }

  if (method == "comb") {
    if (weights == "none") {
      bfcasts <- combinef(pfcasts, gr, weights = FALSE)
    } else if (weights == "sd") {
      wvec <- 1/apply(resid, 2, sd)
      bfcasts <- combinef(pfcasts, gr, weights = TRUE, wvec)
    } else if (weights == "nseries") {
      smat <- as.matrix(smatrix(object))
      wvec <- 1/rowSums(smat)
      bfcasts <- combinef(pfcasts, gr, weights = TRUE, wvec)
    }
    if (keep.fitted) {
      if (weights == "none") {
        fits <- combinef(fits, gr, weights = FALSE)
      } else if (weights == "sd") {
        wvec <- 1/apply(resid, 2, sd)
        fits <- combinef(fits, gr, weights = TRUE, wvec)
      } else if (weights == "nseries") {
        smat <- as.matrix(smatrix(object))
        wvec <- 1/rowSums(smat)
        bfcasts <- combinef(pfcasts, gr, weights = TRUE, wvec)
      }
    }
    if (keep.resid) {
      if (weights == "none") {
        resid <- combinef(resid, gr, weights = FALSE)
      } else if (weights == "sd") {
        wvec <- 1/apply(resid, 2, sd)
        resid <- combinef(resid, gr, weights = TRUE, wvec)
      } else if (weights == "nseries") {
        smat <- as.matrix(smatrix(object))
        wvec <- 1/rowSums(smat)
        bfcasts <- combinef(pfcasts, gr, weights = TRUE, wvec)
      }
    }
  } else if (method == "bu") {
    bfcasts <- pfcasts
  } else if (method == "tdgsa") {
    bfcasts <- TdGsA(pfcasts, object$bts, y)
    if (keep.fitted) {
      fits <- TdGsA(fits, object$bts, y)
    }
    if (keep.resid) {
      resid <- TdGsA(resid, object$bts, y)
    }
  } else if (method == "tdgsf") {
    bfcasts <- TdGsF(pfcasts, object$bts, y)
    if (keep.fitted) {
      fits <- TdGsF(fits, object$bts, y)
    }
    if (keep.resid) {
      resid <- TdGsF(resid, object$bts, y)
    }
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
  } else {
    out <- c(out, groups = list(object$groups))
  }

  return(structure(out, class = class(object)))
}

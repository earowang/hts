forecast.gts <- function(object, h = ifelse(frequency(object) > 1L,
                         2L * frequency(object), 10L), 
                         method = c("comb", "bu", "mo", 
                                    "tdgsa", "tdgsf", "tdfp"),
                         fmethod = c("ets", "arima", "rw"), 
                         keep.fitted = FALSE, keep.resid = FALSE,
                         positive = FALSE, lambda = NULL, level, 
                         weights = c("sd", "none", "nseries"),
                         parallel = FALSE, num.cores = 2,
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
  #   level: Specify level for the middle-out approach, starting with level 0.
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
  if (!is.hts(object) && 
      is.element(method, c("mo", "tdgsf", "tdgsa", "tdfp"))) {
    stop("Argument method is not appropriate for a non-hierarchical time series.")
  }
  if (method == "mo" && missing(level)) {
    stop("Please specify argument level for the middle-out method.")
  }

  # Set up lambda for arg "positive" when lambda is missing
  if (is.null(lambda)) {
    if (positive) {
      if (any(object$bts <= 0L)) {
        stop("All data must be positive.")
      } else {
        lambda <- 0
      }
    } else {
      lambda <- NULL
    }
  }

  if (method == "comb" && weights == "sd") {
    keep.fitted <- TRUE
  }

  # Set up "level" for middle-out
  if (method == "mo") {
    len <- length(object$nodes)
    if (level < 0L || level > len) {
      stop("Argument level is out of the range.")
    } else if (level == 0L) {
      method <- "tdfp"
    } else if (level == len) {
      method <- "bu"
    } else {
      mo.nodes <- object$nodes[level:len]
      level <- seq(level, len)
    }
  }

  # Set up forecast methods
  if (any(method == c("comb", "tdfp"))) { # Combination or tdfp
    y <- aggts(object)  # Grab all ts
  } else if (method == "bu") {  # Bottom-up approach
    y <- object$bts  # Only grab the bts
  } else if (any(method == c("tdgsa", "tdgsf")) && method != "tdfp") {
    y <- aggts(object, levels = 0)  # Grab the top ts
  } else if (method == "mo") {
    y <- aggts(object, levels = level)
  }

  # loop function to grab pf, fitted, resid
  loopfn <- function(x, ...) {  
    out <- list()
    if (fmethod == "ets") {
      models <- ets(x, lambda = lambda, ...)
      out$pfcasts <- forecast(models, h = h, PI = FALSE)$mean
    } else if (fmethod == "arima") {
      models <- auto.arima(x, lambda = lambda, xreg = xreg, 
                           parallel = FALSE, ...)
      out$pfcasts <- forecast(models, h = h, xreg = newxreg, PI = FALSE)$mean
    } else if (fmethod == "rw") {
      models <- rwf(x, h = h, lambda = lambda, ...)
      out$pfcasts <- models$mean
    }
    if (keep.fitted) {
      out$fitted <- fitted(models)
    }
    if (keep.resid) {
      out$resid <- residuals(models)
    }
    return(out)
  }

  if (parallel) { # parallel == TRUE
    if (is.null(num.cores)) {
      num.cores <- detectCores()
    }
    cl <- makeCluster(num.cores)
    loopout <- parSapplyLB(cl = cl, X = y, FUN = function(x) loopfn(x, ...), 
                           simplify = FALSE)
    stopCluster(cl = cl)
  } else {  # parallel = FALSE
    loopout <- lapply(y, function(x) loopfn(x, ...))
  }

  pfcasts <- sapply(loopout, function(x) x$pfcasts)
  if (keep.fitted) {
    fits <- sapply(loopout, function(x) x$fitted)
  }
  if (keep.resid) {
    resid <- sapply(loopout, function(x) x$resid)
  }

  if (is.vector(pfcasts)) {  # if h = 1, sapply returns a vector
    pfcasts <- t(pfcasts)
  }

  # Set up basic info
  tsp.y <- tsp(y)
  bnames <- colnames(object$bts)

  if (method == "comb") {  # Assign class
    class(pfcasts) <- class(object)
    if (keep.fitted) {
      class(fits) <- class(object)
    }
    if (keep.resid) {
      class(resid) <- class(object)
    }
    if (weights == "nseries") {
      if (is.hts(object)) {
        wvec <- InvS4h(object$nodes)
      } else {
        wvec <- InvS4g(object$groups)
      }
    } else if (weights == "sd") {
      resid <- y - fits
      wvec <- 1/sqrt(colMeans(resid^2, na.rm = TRUE))
    }
  }

  # An internal function to call combinef correctly
  Comb <- function(x, ...) {
    if (is.hts(x)) {
      return(combinef(x, nodes = object$nodes, ... ))
    } else {
      return(combinef(x, groups = object$groups, ...))
    }
  }

  if (method == "comb") {
    if (weights == "none") {
      bfcasts <- Comb(pfcasts, keep = "bottom")
    } else if (any(weights == c("sd", "nseries"))) {
      bfcasts <- Comb(pfcasts, weights = wvec, keep = "bottom")
    } 
    if (keep.fitted) {
      if (weights == "none") {
        fits <- Comb(fits, keep = "bottom")
      } else if (any(weights == c("sd", "nseries"))) {
        fits <- Comb(fits, weights = wvec, keep = "bottom")
      } 
    }
    if (keep.resid) {
      if (weights == "none") {
        resid <- Comb(resid, keep = "bottom")
      } else if (any(weights == c("sd", "nseries"))) {
        resid <- Comb(resid, weights = wvec, keep = "bottom")
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
  } else if (method == "tdfp") {
    bfcasts <- TdFp(pfcasts, object$nodes)
    if (keep.fitted) {
      fits <- TdFp(fits, object$nodes)
    }
    if (keep.resid) {
      resid <- TdFp(resid, object$nodes)
    }
  } else if (method == "mo") {
    bfcasts <- MiddleOut(pfcasts, mo.nodes)
    if (keep.fitted) {
      fits <- MiddleOut(fits, mo.nodes)
    }
    if (keep.resid) {
      resid <- MiddleOut(resid, mo.nodes)
    }
  }

  # In case that accuracy.gts() is called later, since NA's have been omitted
  # to ensure slm to run without errors.
  if (method == "comb" && fmethod == "rw" 
      && keep.fitted == TRUE && !is.hts(object)) {
    fits <- rbind(rep(NA, ncol(fits)), fits)
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
    out$fitted <- bfits
  }
  if (exists("bresid")) {
    out$residuals <- bresid
  }
  if (is.hts(object)) {
    out$nodes <- object$nodes
  } else {
    out$groups <- object$groups
  }

  return(structure(out, class = class(object)))
}

forecast.gts <- function(object, h = ifelse(frequency(object) > 1L,
                         2L * frequency(object), 10L), 
                         method = c("comb", "bu", "mo", 
                                    "tdgsa", "tdgsf", "tdfp"),
                         fmethod = c("ets", "arima", "rw"), 
                         keep.fitted = FALSE, keep.resid = FALSE,
                         positive = FALSE, lambda = NULL, level, 
                         weights = c("none", "sd", "nseries"),
                         parallel = FALSE, num.cores = NULL,
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
    stop("Argument method is not appropriate for a non-hierarchical time 
         series.")
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
    if (level < 0L) {
      stop("Argument level must not be negative.")
    } else if (level == 0L) {
      method <- "tdfp"
    } else if (level == length(object$nodes)) {
      method <- "bu"
    } else {
      l.nodes <- length(object$nodes)
      mo.nodes <- object$nodes[level:l.nodes]
      level <- seq(level, l.nodes)
    }
  }

  # Set up forecast methods
  if (method == "comb" || method == "tdfp") { # Combination or tdfp
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
    if (fmethod == "ets") {
      models <- ets(x, lambda = lambda, ...)
      pfcasts <- forecast(models, h = h, PI = FALSE)$mean
    } else if (fmethod == "arima") {
      models <- auto.arima(x, lambda = lambda, xreg = xreg, 
                           parallel = FALSE, ...)
      pfcasts <- forecast(models, h = h, xreg = newxreg, PI = FALSE)$mean
    } else if (fmethod == "rw") {
      models <- rwf(x, h = h, lambda = lambda, ...)
      pfcasts <- models$mean
    }
    out <- list()
    out$pfcasts <- pfcasts
    if (keep.fitted) {
      fits <- fitted(models)
      out$fitted <- fits
    }
    if (keep.resid) {
      resid <- residuals(models)
      out$resid <- resid
    }
    return(out)
  }

  if (parallel) { # parallel == TRUE
    if (is.null(num.cores)) {
      num.cores <- detectCores()
    }
    if (Sys.info()[1] == "Windows") {  # For windows
      cl <- makeCluster(num.cores)
      loopout <- parLapply(cl = cl, X = y, fun = loopfn)
      stopCluster(cl = cl)
    } else {  # For Linux and Mac
      loopout <- mclapply(X = y, FUN = loopfn, mc.cores = num.cores)
    }
  } else {  # parallel = FALSE
    loopout <- lapply(y, loopfn)
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

  if (method == "comb") {
    if (is.hts(object)) {
      gr <- object$nodes
    } else {
      gr <- Smatrix(object)
    }
  }

  if (method == "comb") {
    if (weights == "none") {
      bfcasts <- combinef(pfcasts, gr, keep = "bottom")
    } else if (weights == "sd") {
      resid <- y - fits
      wvec <- 1/apply(resid, 2, sd)
      bfcasts <- combinef(pfcasts, gr, weights = wvec, keep = "bottom")
    } else if (weights == "nseries") {
      smat <- smatrix(object)
      wvec <- 1/rowSums(smat)
      bfcasts <- combinef(pfcasts, gr, weights = wvec, keep = "bottom")
    }
    if (keep.fitted) {
      if (weights == "none") {
        fits <- combinef(fits, gr, keep = "bottom")
      } else if (weights == "sd") {
        resid <- y - fits
        wvec <- 1/apply(resid, 2, sd)
        fits <- combinef(fits, gr, weights = wvec, keep = "bottom")
      } else if (weights == "nseries") {
        smat <- smatrix(object)
        wvec <- 1/rowSums(smat)
        fits <- combinef(fits, gr, weights = wvec, keep = "bottom")
      }
    }
    if (keep.resid) {
      if (weights == "none") {
        resid <- combinef(resid, gr, keep = "bottom")
      } else if (weights == "sd") {
        resid <- y - fits
        wvec <- 1/apply(resid, 2, sd)
        resid <- combinef(resid, gr, weights = wvec, keep = "bottom")
      } else if (weights == "nseries") {
        smat <- smatrix(object)
        wvec <- 1/rowSums(smat)
        resid <- combinef(resid, gr, weights = wvec, keep = "bottom")
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

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
      if (any(object < 0L)) {
        stop("Some data are negative.")
      }
      lambda <- 0
    } else {
      lambda <- NULL
    }
  }

  if (method == "comb" && weights == "sd") {
    keep.resid <- TRUE
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

  if (parallel) { # parallel == TRUE
    if (is.null(num.cores)) {
      num.cores <- detectCores()
    }
    if (Sys.info()[1] == "Windows") {  # For windows
      cl <- makeCluster(num.cores)
      if (fmethod == "ets") {
        models <- parLapply(cl = cl, y, ets, lambda = lambda, ...)
        pfcasts <- parSapply(cl = cl, models, 
                            function(x) forecast(x, h = h, PI = FALSE)$mean,
                            mc.cores = num.cores)
      } else if (fmethod == "arima") {
        models <- parLapply(cl = cl, y, auto.arima, lambda = lambda, 
                            xreg = xreg, parallel = TRUE, ...)
        pfcasts <- parSapply(cl = cl, models,
                            function(x) forecast(x, h = h, xreg = newxreg,
                                                 PI = FALSE)$mean)
      } else if (fmethod == "rw") {
        models <- parLapply(cl = cl, y, rwf, h = h, lambda = lambda, ...)
        pfcasts <- parSapply(cl = cl, models, function(x) x$mean)
      }
      stopCluster(cl = cl)
    } else {  # For Linux and Mac
      if (fmethod == "ets") {
        models <- mclapply(y, ets, lambda = lambda, ..., mc.cores = num.cores)
        pfcasts <- mclapply(models, 
                            function(x) forecast(x, h = h, PI = FALSE)$mean,
                            mc.cores = num.cores)
        pfcasts <- matrix(unlist(pfcasts), nrow = h)
      } else if (fmethod == "arima") {
        models <- mclapply(y, auto.arima, lambda = lambda, xreg = xreg,
                           parallel = TRUE, ..., mc.cores = num.cores)
        pfcasts <- mclapply(models,
                            function(x) forecast(x, h = h, xreg = newxreg,
                            PI = FALSE)$mean)
        pfcasts <- matrix(unlist(pfcasts), nrow = h)
      } else if (fmethod == "rw") {
        models <- mclapply(y, rwf, h = h, lambda = lambda, ...)
        pfcasts <- sapply(models, function(x) x$mean)
      }
    }
  } else {
    if (fmethod == "ets") {
      models <- lapply(y, ets, lambda = lambda, ...)
      pfcasts <- sapply(models, function(x) forecast(x, h = h, PI = FALSE)$mean)
    } else if (fmethod == "arima") {
      models <- lapply(y, auto.arima, lambda = lambda, xreg = xreg, ...)
      pfcasts <- sapply(models, function(x) forecast(x, h = h, xreg = newxreg,
                        PI = FALSE)$mean)
    } else if (fmethod == "rw") {
      models <- lapply(y, rwf, h = h, lambda = lambda, ...)
      pfcasts <- sapply(models, function(x) x$mean)
    }
  }
  if (keep.fitted) {
    fits <- sapply(models, fitted)
  }
  if (keep.resid) {
    resid <- sapply(models, residuals)
  }


  if (is.vector(pfcasts)) {  # if h = 1, sapply returns a vector
    pfcasts <- t(pfcasts)
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
      gr <- Smatrix(object)
    }
  }

  if (method == "comb") {
    if (weights == "none") {
      bfcasts <- combinef(pfcasts, gr)
    } else if (weights == "sd") {
      wvec <- 1/apply(resid, 2, sd)
      bfcasts <- combinef(pfcasts, gr, weights = wvec)
    } else if (weights == "nseries") {
      smat <- smatrix(object)
      wvec <- 1/rowSums(smat)
      bfcasts <- combinef(pfcasts, gr, weights = wvec)
    }
    if (keep.fitted) {
      if (weights == "none") {
        fits <- combinef(fits, gr)
      } else if (weights == "sd") {
        wvec <- 1/apply(resid, 2, sd)
        fits <- combinef(fits, gr, weights = wvec)
      } else if (weights == "nseries") {
        smat <- smatrix(object)
        wvec <- 1/rowSums(smat)
        fits <- combinef(fits, gr, weights = wvec)
      }
    }
    if (keep.resid) {
      if (weights == "none") {
        resid <- combinef(resid, gr)
      } else if (weights == "sd") {
        wvec <- 1/apply(resid, 2, sd)
        resid <- combinef(resid, gr, weights = wvec)
      } else if (weights == "nseries") {
        smat <- smatrix(object)
        wvec <- 1/rowSums(smat)
        resid <- combinef(resid, gr, weights = wvec)
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

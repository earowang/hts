forecast.gts <- function(object, h = ifelse(frequency(object$bts) > 1L,
                         2L * frequency(object$bts), 10L),
                         method = c("comb", "bu", "mo","tdgsa", "tdgsf", "tdfp"),
                         weights = c("wls", "ols", "mint", "nseries"),
                         fmethod = c("ets", "arima", "rw"),
                         algorithms = c("lu", "cg", "chol", "recursive", "slm"),
                         covariance = c("shr", "sam"),
                         keep.fitted = FALSE, keep.resid = FALSE,
                         positive = FALSE, lambda = NULL, level,
                         parallel = FALSE, num.cores = 2, FUN = NULL,
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
  # Recode old weights arguments
  if(length(weights)==1L)
  {
    if(weights=="sd")
      weights <- "wls"
    else if(weights=="none")
      weights <- "ols"
  }
  weights <- match.arg(weights)
  covariance <- match.arg(covariance)
  alg <- match.arg(algorithms)
  if (is.null(FUN)) {
    fmethod <- match.arg(fmethod)
  }
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
      if (any(object$bts <= 0L, na.rm=FALSE)) {
        stop("All data must be positive.")
      } else {
        lambda <- 0
      }
    } else {
      lambda <- NULL
    }
  }

  # Remember the original keep.fitted argument for later
  keep.fitted0 <- keep.fitted
  if (method=="comb" && (weights == "mint" || weights == "wls")) {
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
    if (is.null(FUN)) {
      if (fmethod == "ets") {
        models <- ets(x, lambda = lambda, ...)
        out$pfcasts <- forecast(models, h = h, PI = FALSE)$mean
      } else if (fmethod == "arima") {
        models <- auto.arima(x, lambda = lambda, xreg = xreg,
                             parallel = FALSE, ...)
        out$pfcasts <- forecast(models, h = h, xreg = newxreg)$mean
      } else if (fmethod == "rw") {
        models <- rwf(x, h = h, lambda = lambda, ...)
        out$pfcasts <- models$mean
      }
    } else { # user defined function to produce point forecasts
      models <- FUN(x, ...)
      out$pfcasts <- forecast(models, h = h)$mean
    }
    if (keep.fitted) {
      out$fitted <- stats::fitted(models)
    }
    if (keep.resid) {
      out$resid <- stats::residuals(models)
    }
    return(out)
  }

  if (parallel) { # parallel == TRUE
    if (is.null(num.cores)) {
      num.cores <- detectCores()
    }
    # Parallel start new process
    lambda <- lambda
    xreg <- xreg
    newxreg <- newxreg
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
  tsp.y <- stats::tsp(y)
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
    } else if (weights == "wls") {
      tmp.resid <- y - fits # it ensures resids are additive errors
      wvec <- 1/sqrt(colMeans(tmp.resid^2, na.rm = TRUE))
    }
    else if (weights == "mint") {
      tmp.resid <- stats::na.omit(y - fits)
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

  # An internal function to call MinT correctly
  mint <- function(x, ...) {
    if (is.hts(x)) {
      return(MinT(x, nodes = object$nodes, ... ))
    } else {
      return(MinT(x, groups = object$groups, ...))
    }
  }

  if (method == "comb") {
    if (weights == "ols") {
      bfcasts <- Comb(pfcasts, keep = "bottom", algorithms = alg)
    } else if (any(weights == c("wls", "nseries"))) {
      bfcasts <- Comb(pfcasts, weights = wvec, keep = "bottom",
                      algorithms = alg)
    } else { # weights=="mint"
      bfcasts <- mint(pfcasts, residual = tmp.resid,
                    covariance = covariance, keep = "bottom", algorithms = alg)
    }
    if (keep.fitted0) {
      if (weights == "ols") {
        fits <- Comb(fits, keep = "bottom", algorithms = alg)
      } else if (any(weights == c("wls", "nseries"))) {
        fits <- Comb(fits, weights = wvec, keep = "bottom",
                     algorithms = alg)
      } else if(weights=="mint") {
        fits <- mint(fits, residual = tmp.resid,
                   covariance = covariance, keep = "bottom", algorithms = alg)
      }
    }
    if (keep.resid) {
      if (weights == "ols") {
        resid <- Comb(resid, keep = "bottom", algorithms = alg)
      } else if (any(weights == c("wls", "nseries"))) {
        resid <- Comb(resid, weights = wvec, keep = "bottom",
                      algorithms = alg)
      } else if (weights=="mint") {
        resid <- mint(resid, residual = tmp.resid,
                    covariance = covariance, keep = "bottom", algorithms = alg)
      }
    }
  } else if (method == "bu") {
    bfcasts <- pfcasts
  } else if (method == "tdgsa") {
    bfcasts <- TdGsA(pfcasts, object$bts, y)
    if (keep.fitted0) {
      fits <- TdGsA(fits, object$bts, y)
    }
    if (keep.resid) {
      resid <- TdGsA(resid, object$bts, y)
    }
  } else if (method == "tdgsf") {
    bfcasts <- TdGsF(pfcasts, object$bts, y)
    if (keep.fitted0) {
      fits <- TdGsF(fits, object$bts, y)
    }
    if (keep.resid) {
      resid <- TdGsF(resid, object$bts, y)
    }
  } else if (method == "tdfp") {
    bfcasts <- TdFp(pfcasts, object$nodes)
    if (keep.fitted0) {
      fits <- TdFp(fits, object$nodes)
    }
    if (keep.resid) {
      resid <- TdFp(resid, object$nodes)
    }
  } else if (method == "mo") {
    bfcasts <- MiddleOut(pfcasts, mo.nodes)
    if (keep.fitted0) {
      fits <- MiddleOut(fits, mo.nodes)
    }
    if (keep.resid) {
      resid <- MiddleOut(resid, mo.nodes)
    }
  }

  # In case that accuracy.gts() is called later, since NA's have been omitted
  # to ensure slm/chol to run without errors.
  if (method == "comb" && fmethod == "rw"
      && keep.fitted0 == TRUE && (alg == "slm" || alg == "chol")) {
    fits <- rbind(rep(NA, ncol(fits)), fits)
  }

  bfcasts <- ts(bfcasts, start = tsp.y[2L] + 1L/tsp.y[3L],
                frequency = tsp.y[3L])
  colnames(bfcasts) <- bnames
  class(bfcasts) <- class(object$bts)
  attr(bfcasts, "msts") <- attr(object$bts, "msts")

  if (keep.fitted0) {
    bfits <- ts(fits, start = tsp.y[1L], frequency = tsp.y[3L])
    colnames(bfits) <- bnames
  }
  if (keep.resid) {
    bresid <- ts(resid, start = tsp.y[1L], frequency = tsp.y[3L])
    colnames(bresid) <- bnames
  }

  # Output
  out <- list(bts = bfcasts, histy = object$bts, labels = object$labels,
              method = method, fmethod = fmethod)
  if (keep.fitted0) {
    out$fitted <- bfits
  }
  if (keep.resid) {
    out$residuals <- bresid
  }
  if (is.hts(object)) {
    out$nodes <- object$nodes
  } else {
    out$groups <- object$groups
  }

  return(structure(out, class = class(object)))
}

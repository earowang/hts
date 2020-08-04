#' Forecast a hierarchical or grouped time series
#' 
#' Methods for forecasting hierarchical or grouped time series.
#' 
#' Base methods implemented include ETS, ARIMA and the naive (random walk)
#' models. Forecasts are distributed in the hierarchy using bottom-up,
#' top-down, middle-out and optimal combination methods.
#' 
#' Three top-down methods are available: the two Gross-Sohl methods and the
#' forecast-proportion approach of Hyndman, Ahmed, and Athanasopoulos (2011).
#' The "middle-out" method \code{"mo"} uses bottom-up (\code{"bu"}) for levels
#' higher than \code{level} and top-down forecast proportions (\code{"tdfp"})
#' for levels lower than \code{level}.
#' 
#' For non-hierarchical grouped data, only bottom-up and combination methods
#' are possible, as any method involving top-down disaggregation requires a
#' hierarchical ordering of groups.
#' 
#' When \code{xreg} and \code{newxreg} are passed, the same covariates are
#' applied to every series in the hierarchy.
#' 
#' The \code{control.nn} argument is a list that can supply any of the following components:
#' \describe{
#' \item{\code{ptype}}{Permutation method to be used: \code{"fixed"}  or \code{"random"}. Defaults to \code{"fixed"}.}
#' \item{\code{par}}{The number of full exchange rules that may be tried. Defaults to 10.}
#' \item{\code{gtol}}{The tolerance of the convergence criteria. Defaults to \code{sqrt(.Machine$double.eps)}.}
#' }
#' 
#' @aliases forecast.gts forecast.hts
#' @param object Hierarchical or grouped time series object of class
#' \code{{gts}}
#' @param h Forecast horizon
#' @param method Method for distributing forecasts within the hierarchy. See
#' details
#' @param weights Weights used for "optimal combination" method:
#' \code{weights="ols"} uses an unweighted combination (as described in Hyndman
#' et al 2011); \code{weights="wls"} uses weights based on forecast variances
#' (as described in Hyndman et al 2016); \code{weights="mint"} uses a full
#' covariance estimate to determine the weights (as described in Wickramasuriya et al
#' 2019); \code{weights="nseries"} uses weights based on the number of series
#' aggregated at each node.
#' @param fmethod Forecasting method to use for each series.
#' @param algorithms An algorithm to be used for computing the combination
#' forecasts (when \code{method=="comb"}). The combination forecasts are based
#' on an ill-conditioned regression model. "lu" indicates LU decomposition is
#' used; "cg" indicates a conjugate gradient method; "chol" corresponds to a
#' Cholesky decomposition; "recursive" indicates the recursive hierarchical
#' algorithm of Hyndman et al (2016); "slm" uses sparse linear regression. Note
#' that \code{algorithms = "recursive"} and \code{algorithms = "slm"} cannot be
#' used if \code{weights="mint"}.
#' @param covariance Type of the covariance matrix to be used with
#' \code{weights="mint"}: either a shrinkage estimator (\code{"shr"}) with
#' shrinkage towards the diagonal; or a sample covariance matrix
#' (\code{"sam"}).
#' @param nonnegative Logical. Should the reconciled forecasts be non-negative?
#' @param control.nn A list of control parameters to be passed on to the 
#' block principal pivoting algorithm. See 'Details'.
#' @param keep.fitted If \code{TRUE}, keep fitted values at the bottom level.
#' @param keep.resid If \code{TRUE}, keep residuals at the bottom level.
#' @param positive If \code{TRUE}, forecasts are forced to be strictly positive (by
#' setting \code{lambda=0}).
#' @param lambda Box-Cox transformation parameter.
#' @param level Level used for "middle-out" method (only used when \code{method
#' = "mo"}).
#' @param FUN A user-defined function that returns an object which can be
#' passed to the \code{forecast} function. It is applied to all series in order
#' to generate base forecasts.  When \code{FUN} is not \code{NULL},
#' \code{fmethod}, \code{positive} and \code{lambda} are all ignored. Suitable
#' values for \code{FUN} are \code{\link[forecast]{tbats}} and
#' \code{\link[forecast]{stlf}} for example.
#' @param xreg When \code{fmethod = "arima"}, a vector or matrix of external
#' regressors used for modelling, which must have the same number of rows as
#' the original univariate time series
#' @param newxreg When \code{fmethod = "arima"}, a vector or matrix of external
#' regressors used for forecasting, which must have the same number of rows as
#' the \code{h} forecast horizon
#' @param parallel If \code{TRUE}, import \code{parallel} package to allow parallel
#' processing.
#' @param num.cores If \code{parallel = TRUE}, specify how many cores are going to be
#' used.
#' @param ... Other arguments passed to \code{\link[forecast]{ets}},
#' \code{\link[forecast]{auto.arima}} or \code{FUN}.
#' @return A forecasted hierarchical/grouped time series of class \code{gts}.
#' @note In-sample fitted values and resiuals are not returned if \code{method = "comb"} and \code{nonnegative = TRUE}.
#' @author Earo Wang, Rob J Hyndman and Shanika L Wickramasuriya
#' @seealso \code{\link[hts]{hts}}, \code{\link[hts]{gts}},
#' \code{\link[hts]{plot.gts}}, \code{\link[hts]{accuracy.gts}}
#' @references Athanasopoulos, G., Ahmed, R. A., & Hyndman, R. J. (2009).
#' Hierarchical forecasts for Australian domestic tourism, \emph{International
#' Journal of Forecasting}, \bold{25}, 146-166.
#' 
#' Hyndman, R. J., Ahmed, R. A., Athanasopoulos, G., & Shang, H. L. (2011). Optimal
#' combination forecasts for hierarchical time series. \emph{Computational
#' Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
#' \url{http://robjhyndman.com/papers/hierarchical/}
#' 
#' Hyndman, R. J., Lee, A., & Wang, E. (2016). Fast computation of reconciled
#' forecasts for hierarchical and grouped time series. \emph{Computational
#' Statistics and Data Analysis}, \bold{97}, 16--32.
#' \url{http://robjhyndman.com/papers/hgts/}
#' 
#' Wickramasuriya, S. L., Athanasopoulos, G., & Hyndman, R. J. (2019).
#' Optimal forecast reconciliation for hierarchical and grouped time series through trace minimization.
#' \emph{Journal of the American Statistical Association}, \bold{114}(526), 804--819. \url{http://robjhyndman.com/working-papers/mint/}
#' 
#' Wickramasuriya, S. L., Turlach, B. A., & Hyndman, R. J. (to appear). Optimal non-negative forecast reconciliation. 
#' \emph{Statistics and Computing}. \url{https://robjhyndman.com/publications/nnmint/}
#' 
#' Gross, C., & Sohl, J. (1990). Dissagregation methods to expedite product
#' line forecasting, \emph{Journal of Forecasting}, \bold{9}, 233--254.
#' @keywords ts
#' @method forecast gts
#' @examples
#' 
#' forecast(htseg1, h = 10, method = "bu", fmethod = "arima")
#' 
#' \dontrun{
#'   forecast(
#'     htseg2, h = 10, method = "comb", algorithms = "lu",
#'     FUN = function(x) tbats(x, use.parallel = FALSE)
#'   )
#' }
#' 
#' @export
#' @export forecast.gts
forecast.gts <- function(
  object, 
  h = ifelse(frequency(object$bts) > 1L, 2L * frequency(object$bts), 10L),
  method = c("comb", "bu", "mo","tdgsa", "tdgsf", "tdfp"),
  weights = c("wls", "ols", "mint", "nseries"),
  fmethod = c("ets", "arima", "rw"),
  algorithms = c("lu", "cg", "chol", "recursive", "slm"),
  covariance = c("shr", "sam"),
  nonnegative = FALSE, control.nn = list(),
  keep.fitted = FALSE, keep.resid = FALSE,
  positive = FALSE, lambda = NULL, level, FUN = NULL,
  xreg = NULL, newxreg = NULL, parallel = FALSE, num.cores = 2, ...
) {
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
    stop("Argument object must be either a hts or gts object.", call. = FALSE)
  }
  if (h < 1L) {
    stop("Argument h must be positive.", call. = FALSE)
  }
  if (!is.hts(object) &&
      is.element(method, c("mo", "tdgsf", "tdgsa", "tdfp"))) {
    stop("Argument method is not appropriate for a non-hierarchical time series.", call. = FALSE)
  }
  if (method == "mo" && missing(level)) {
    stop("Please specify argument level for the middle-out method.", call. = FALSE)
  }
  if (is.element(method, c("bu", "mo","tdgsa", "tdgsf", "tdfp")) && nonnegative) {
    stop("Non-negative algorithm is only implemented for combination forecasts.", call. = FALSE)
  }

  # Set up lambda for arg "positive" when lambda is missing
  if (is.null(lambda)) {
    if (positive) {
      if (any(object$bts <= 0L, na.rm=FALSE)) {
        stop("All data must be positive.", call. = FALSE)
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
      stop("Argument level is out of the range.", call. = FALSE)
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
      if (is.null(newxreg)) {
        out$pfcasts <- forecast(models, h = h)$mean
      } else {
        out$pfcasts <- forecast(models, h = h, xreg = newxreg)$mean
      }
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
  
  if (any(pfcasts < 0) && nonnegative) {
    pfcasts[pfcasts < 0] <- 0
    warning("Negative base forecasts are truncated to zero.")
  }
  
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
      wvec <- 1/colMeans(tmp.resid^2, na.rm = TRUE)
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
      bfcasts <- Comb(pfcasts, nonnegative = nonnegative,
                      parallel = parallel, num.cores = num.cores,
                      keep = "bottom", algorithms = alg, control.nn = control.nn)
    } else if (any(weights == c("wls", "nseries"))) {
      bfcasts <- Comb(pfcasts, weights = wvec, nonnegative = nonnegative,
                      parallel = parallel, num.cores = num.cores,
                      keep = "bottom", algorithms = alg, control.nn = control.nn)
    } else { # weights=="mint"
      bfcasts <- mint(pfcasts, residual = tmp.resid,
                    covariance = covariance, nonnegative = nonnegative,
                    parallel = parallel, num.cores = num.cores, 
                    keep = "bottom", algorithms = alg, control.nn = control.nn)
    }
    if (keep.fitted0 && !nonnegative) {
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
    if (keep.resid && !nonnegative) {
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
  if (method == "comb" && fmethod == "rw" && !nonnegative
      && keep.fitted0 == TRUE && (alg == "slm" || alg == "chol")) {
    fits <- rbind(rep(NA, ncol(fits)), fits)
  }

  bfcasts <- ts(bfcasts, start = tsp.y[2L] + 1L/tsp.y[3L],
                frequency = tsp.y[3L])
  colnames(bfcasts) <- bnames
  class(bfcasts) <- class(object$bts)
  attr(bfcasts, "msts") <- attr(object$bts, "msts")

  if (keep.fitted0 && !nonnegative) {
    bfits <- ts(fits, start = tsp.y[1L], frequency = tsp.y[3L])
    colnames(bfits) <- bnames
  }
  if (keep.resid && !nonnegative) {
    bresid <- ts(resid, start = tsp.y[1L], frequency = tsp.y[3L])
    colnames(bresid) <- bnames
  }

  # Output
  out <- list(bts = bfcasts, histy = object$bts, labels = object$labels,
              method = method, fmethod = fmethod)
  if (keep.fitted0 && !nonnegative) {
    out$fitted <- bfits
  }
  if (keep.resid && !nonnegative) {
    out$residuals <- bresid
  }
  if (is.hts(object)) {
    out$nodes <- object$nodes
  } else {
    out$groups <- object$groups
  }

  return(structure(out, class = class(object)))
}

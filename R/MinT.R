## Arguments

# x: Matrix of insample residuals for all time series in the hierarchy. Each column referring to one time series.

# Target matrix for shrinking towards a diagonal matrix
lowerD <- function(x)
{
  n <- nrow(x)
  return(diag(apply(x, 2, crossprod) / n))
}

## Arguments

# x: Matrix of insample residuals for all time series in the hierarchy. Each column referring to one time series.
# tar: Lower dimensional matrix.

# Shrinked covariance matrix - Schafer and strimmer approach
shrink.estim <- function(x, tar)
{
  if (is.matrix(x) == TRUE && is.numeric(x) == FALSE)
    stop("The data matrix must be numeric!", call. = FALSE)
  p <- ncol(x)
  n <- nrow(x)
  covm <- crossprod(x) / n
  corm <- cov2cor(covm)
  xs <- scale(x, center = FALSE, scale = sqrt(diag(covm)))
  v <- (1/(n * (n - 1))) * (crossprod(xs^2) - 1/n * (crossprod(xs))^2)
  diag(v) <- 0
  corapn <- cov2cor(tar)
  d <- (corm - corapn)^2
  lambda <- sum(v)/sum(d)
  lambda <- max(min(lambda, 1), 0)
  shrink.cov <- lambda * tar + (1 - lambda) * covm
  return(list(shrink.cov, c("The shrinkage intensity lambda is:",
                            round(lambda, digits = 4))))
}

# MinT - Trace minimization approach


#' Trace minimization for hierarchical or grouped time series
#' 
#' Using the method of Wickramasuriya et al. (2019), this function combines the
#' forecasts at all levels of a hierarchical or grouped time series. The
#' \code{\link{forecast.gts}} calls this function when the \code{MinT} method
#' is selected.
#' 
#' @param fcasts Matrix of forecasts for all levels of a hierarchical or
#' grouped time series. Each row represents one forecast horizon and each
#' column represents one time series of aggregated or disaggregated forecasts.
#' @param nodes If the object class is hts, a list contains the number of child
#' nodes referring to hts.
#' @param groups If the object is gts, a gmatrix is required, which is the same
#' as groups in the function gts.
#' @param residual Matrix of insample residuals for all the aggregated and
#' disaggregated time series. The columns must be in the same order as
#' \code{fcasts}.
#' @param covariance Type of the covariance matrix to be used. Shrinking
#' towards a diagonal unequal variances (\code{"shr"}) or sample covariance matrix
#' (\code{"sam"}).
#' @param nonnegative Logical. Should the reconciled forecasts be non-negative?
#' @param algorithms Algorithm used to compute inverse of the matrices.
#' @param keep Return a gts object or the reconciled forecasts at the bottom
#' level.
#' @param parallel Logical. Import parallel package to allow parallel processing.
#' @param num.cores Numeric. Specify how many cores are going to be used.
#' @param control.nn A list of control parameters to be passed on to the 
#' block principal pivoting algorithm. See 'Details'.
#' @return Return the reconciled \code{gts} object or forecasts at the bottom
#' level.
#' @details 
#' The \code{control.nn} argument is a list that can supply any of the following components: 
#' \describe{
#' \item{\code{ptype}}{Permutation method to be used: \code{"fixed"} or \code{"random"}. Defaults to \code{"fixed"}.}
#' \item{\code{par}}{The number of full exchange rules that may be tried. Defaults to 10.} 
#' \item{\code{gtol}}{The tolerance of the convergence criteria. Defaults to \code{sqrt(.Machine$double.eps)}.}
#' }
#' @author Shanika L Wickramasuriya
#' @seealso \code{\link[hts]{hts}}, \code{\link[hts]{gts}},
#' \code{\link[hts]{forecast.gts}}, \code{\link[hts]{combinef}}
#' @references Wickramasuriya, S. L., Athanasopoulos, G., & Hyndman, R. J. (2019).
#' Optimal forecast reconciliation for hierarchical and grouped time series through trace minimization.
#' \emph{Journal of the American Statistical Association}, \bold{114}(526), 804--819. \url{http://robjhyndman.com/working-papers/mint/}
#' 
#' Wickramasuriya, S. L., Turlach, B. A., & Hyndman, R. J. (to appear). Optimal non-negative forecast reconciliation. 
#' \emph{Statistics and Computing}. \url{https://robjhyndman.com/publications/nnmint/}
#' 
#' Hyndman, R. J., Lee, A., & Wang, E. (2016).  Fast computation of reconciled
#' forecasts for hierarchical and grouped time series.  \emph{Computational
#' Statistics and Data Analysis}, \bold{97}, 16--32.
#' \url{http://robjhyndman.com/papers/hgts/}
#' @keywords ts
#' @examples
#' 
#' # hts example
#' \dontrun{
#' h <- 12 
#' ally <- aggts(htseg1)
#' n <- nrow(ally)
#' p <- ncol(ally)
#' allf <- matrix(NA, nrow = h, ncol = p)
#' res <- matrix(NA, nrow = n, ncol = p)
#' for(i in 1:p)
#' {
#'   fit <- auto.arima(ally[, i])
#'   allf[, i] <- forecast(fit, h = h)$mean
#'   res[, i] <- na.omit(ally[, i] - fitted(fit))
#' }
#' allf <- ts(allf, start = 51)
#' y.f <- MinT(allf, get_nodes(htseg1), residual = res, covariance = "shr", 
#'   keep = "gts", algorithms = "lu")
#' plot(y.f)
#' y.f_cg <- MinT(allf, get_nodes(htseg1), residual = res, covariance = "shr", 
#'   keep = "all", algorithms = "cg")
#' }
#' 
#' \dontrun{
#' h <- 12
#' ally <- abs(aggts(htseg2))
#' allf <- matrix(NA, nrow = h, ncol = ncol(ally))
#' res <- matrix(NA, nrow = nrow(ally), ncol = ncol(ally))
#' for(i in 1:ncol(ally)) {
#'   fit <- auto.arima(ally[, i], lambda = 0, biasadj = TRUE)
#'   allf[,i] <- forecast(fit, h = h)$mean
#'   res[,i] <- na.omit(ally[, i] - fitted(fit))
#' }
#' b.f <- MinT(allf, get_nodes(htseg2), residual = res, covariance = "shr",
#'   keep = "bottom", algorithms = "lu")
#' b.nnf <-  MinT(allf, get_nodes(htseg2), residual = res, covariance = "shr",
#'   keep = "bottom", algorithms = "lu", nonnegative = TRUE, parallel = TRUE)
#' }
#'   
#' # gts example
#' \dontrun{
#' abc <- ts(5 + matrix(sort(rnorm(200)), ncol = 4, nrow = 50))
#' g <- rbind(c(1,1,2,2), c(1,2,1,2))
#' y <- gts(abc, groups = g)
#' h <- 12
#' ally <- aggts(y)
#' n <- nrow(ally)
#' p <- ncol(ally)
#' allf <- matrix(NA,nrow = h,ncol = ncol(ally))
#' res <- matrix(NA, nrow = n, ncol = p)
#' for(i in 1:p)
#' {
#'   fit <- auto.arima(ally[, i])
#'   allf[, i] <- forecast(fit, h = h)$mean
#'   res[, i] <- na.omit(ally[, i] - fitted(fit))
#' }
#' allf <- ts(allf, start = 51)
#' y.f <- MinT(allf, groups = get_groups(y), residual = res, covariance = "shr", 
#'   keep = "gts", algorithms = "lu")
#' plot(y.f)
#' }
#' @export MinT
MinT <- function (fcasts, nodes = NULL, groups = NULL, residual, covariance = c("shr", "sam"),
                  nonnegative = FALSE, algorithms = c("lu", "cg", "chol"), 
                  keep = c("gts", "all", "bottom"),  parallel = FALSE, num.cores = 2, control.nn = list())
{
  if (is.null(nodes) && is.null(groups)) {
    stop("Please specify the hierarchical or the grouping structure.", call. = FALSE)
  }
  
  if (!xor(is.null(nodes), is.null(groups))) {
    stop("Please specify either nodes or groups argument, not both.", call. = FALSE)
  }
  
  alg <- match.arg(algorithms)
  keep <- match.arg(keep)
  covar <- match.arg(covariance)
  res <- residual
  fcasts <- stats::as.ts(fcasts)
  tspx <- stats::tsp(fcasts)
  cnames <- colnames(fcasts)
  
  if (!nonnegative) {
    if (missing(residual))
    {
      stop("MinT needs insample residuals.", call. = FALSE)
    }
    if (covar=="sam")
    {
      n <- nrow(res)
      w.1 <- crossprod(res) / n
      if(is.positive.definite(w.1)==FALSE)
      {
        stop("MinT needs covariance matrix to be positive definite.", call. = FALSE)
      }
    } else {
      tar <- lowerD(res)
      shrink <- shrink.estim(res, tar)
      w.1 <- shrink[[1]]
      lambda <- shrink[[2]]
      if (is.positive.definite(w.1)==FALSE)
      {
        stop("MinT needs covariance matrix to be positive definite.", call. = FALSE)
      }
    }
    
    if (is.null(groups)) { # hts class
      totalts <- sum(Mnodes(nodes))
      if (!is.matrix(fcasts)) {
        fcasts <- t(fcasts)
      }
      h <- nrow(fcasts)
      if (ncol(fcasts) != totalts) {
        stop("Argument fcasts requires all the forecasts.", call. = FALSE)
      }
      gmat <- GmatrixH(nodes)
      fcasts <- t(fcasts)
      if (alg == "chol") {
        smat <- Smatrix(gmat)
        if (!is.null(w.1)) {
          w.1 <- as.matrix.csr(w.1)
        }
        allf <- CHOL(fcasts = fcasts, S = smat, weights = w.1, allow.changes = FALSE)
      }
      else {
        smat <- SmatrixM(gmat)
        if (!is.null(w.1)) {
          weights <-  methods::as(w.1, "sparseMatrix")
        }
        if (alg == "lu") {
          allf <- LU(fcasts = fcasts, S = smat, weights = weights, allow.changes = FALSE)
        }
        else if (alg == "cg") {
          allf <- CG(fcasts = fcasts, S = smat, weights = weights, allow.changes = FALSE)
        }
      }
      
      if (keep == "all") {
        out <- t(allf)
      }
      else {
        bottom <- totalts - (ncol(smat):1L) + 1L
        bf <- t(allf[bottom, ])
        if (keep == "gts") {
          bf <- ts(bf, start = tspx[1L], frequency = tspx[3L])
          out <- suppressMessages(hts(bf, nodes = nodes))
        }
        else {
          out <- bf
        }
      }
    }
    else if (is.null(nodes)) {
      rownames(groups) <- NULL
      gmat <- GmatrixG(groups)
      totalts <- sum(Mlevel(gmat))
      if (ncol(fcasts) != totalts) {
        stop("Argument fcasts requires all the forecasts.", call. = FALSE)
      }
      fcasts <- t(fcasts)
      if (alg == "chol") {
        smat <- Smatrix(gmat)
        if (!is.null(w.1)) {
          weights <- as.matrix.csr(w.1)
        }
        allf <- CHOL(fcasts = fcasts, S = smat, weights = weights, allow.changes = FALSE)
      }
      else {
        smat <- SmatrixM(gmat)
        if (!is.null(w.1)) {
          weights <-  methods::as(w.1, "sparseMatrix")
        }
        if (alg == "lu") {
          allf <- LU(fcasts = fcasts, S = smat, weights = weights, allow.changes = FALSE)
        }
        else if (alg == "cg") {
          allf <- CG(fcasts = fcasts, S = smat, weights = weights, allow.changes = FALSE)
        }
      }
      if (keep == "all") {
        out <- t(allf)
      }
      else {
        bottom <- totalts - (ncol(smat):1L) + 1L
        bf <- t(allf[bottom, ])
        if (keep == "gts") {
          colnames(bf) <- cnames[bottom]
          bf <- ts(bf, start = tspx[1L], frequency = tspx[3L])
          out <- suppressMessages(gts(bf, groups = groups))
        }
        else {
          out <- bf
        }
      }
    }
  } else {
    if (any(fcasts < 0)) {
      fcasts[fcasts < 0] <- 0
      warning("Negative base forecasts are truncated to zero.")
    }
    
    lst.fc <- split(fcasts, row(fcasts))
    if (parallel) {
      if (is.null(num.cores)) {
        num.cores <- detectCores()
      }
      cl <- makeCluster(num.cores)
      bf <- parSapplyLB(cl = cl, X = lst.fc, MinTbpv, nodes = nodes, groups = groups, res = res, covar = covar, alg = alg, control.nn = control.nn, simplify = TRUE)
      stopCluster(cl = cl)
    } else {
      bf <- sapply(lst.fc, MinTbpv, nodes = nodes, groups = groups, res = res, covar = covar, alg = alg, control.nn = control.nn)
    }
    bf <- ts(t(bf), start = tspx[1L], frequency = tspx[3L])
    if (is.null(groups)) {
      if (keep == "bottom") {
        out <- bf
      } else {
        out <- suppressMessages(hts(bf, nodes = nodes))
        if (keep == "all") {
          out <- aggts(out)
        }
      }
    } else {
      if (keep == "bottom") {
        out <- bf
      } else {
        colnames(bf) <- tail(cnames, ncol(bf))
        out <- suppressMessages(gts(bf, groups = groups))
        if (keep == "all") {
          out <- aggts(out)
        }
      }
    }
  }
  return(out)
}



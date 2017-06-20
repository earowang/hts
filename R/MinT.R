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

## Arguments

#fcasts: Matrix of forecasts for all levels of the hierarchical time series.
#        Each row represents one forecast horizon and each column represents one time series from the hierarchy

# nodes: If the object class is hts, a list contains the number of child nodes referring to hts.
# groups: If the object is gts, a gmatrix is required, which is the same as groups in the function gts.
# residuals: Matrix of insample residuals for all time series in the hierarchy. Each column referring to one time series.
# covariance: Type of the covariance matrix to be used. Sample covariance matrix ("sam") or shrinking towards a diagonal unequal variances ("shr").
# algorithms: Algorithm used to compute inverse of the matrices.
# keep: Return a gts object or the reconciled forecasts at the bottom level.


# MinT - Trace minimization approach


#' Trace minimization for hierarchical or grouped time series
#' 
#' Using the method of Wickramasuriya et al. (2015), this function combines the
#' forecasts at all levels of a hierarchical or grouped time series. The
#' \code{\link{forecast.gts}} calls this function when the \code{MinT} method
#' is selected.
#' 
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
#' towards a diagonal unequal variances ("shr") or sample covariance matrix
#' ("sam").
#' @param algorithms Algorithm used to compute inverse of the matrices.
#' @param keep Return a gts object or the reconciled forecasts at the bottom
#' level.
#' @return Return the reconciled \code{gts} object or forecasts at the bottom
#' level.
#' @author Shanika L Wickramasuriya
#' @seealso \code{\link[hts]{hts}}, \code{\link[hts]{gts}},
#' \code{\link[hts]{forecast.gts}}, \code{\link[hts]{combinef}}
#' @references Wickramasuriya, S. L., Athanasopoulos, G., & Hyndman, R. J.
#' (2015).  Forecasting hierarchical and grouped time series through trace
#' minimization.  \emph{Working paper 15/15, Department of Econometrics &
#' Business Statistics, Monash University.}
#' \url{http://robjhyndman.com/working-papers/mint/}
#' 
#' Hyndman, R. J., Lee, A., & Wang, E. (2015).  Fast computation of reconciled
#' forecasts for hierarchical and grouped time series.  \emph{Computational
#' Statistics and Data Analysis}, \bold{97}, 16--32.
#' \url{http://robjhyndman.com/papers/hgts/}
#' @keywords ts
#' @examples
#' 
#' # hts example
#' \dontrun{h <- 12 
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
#' # gts example
#' \dontrun{abc <- ts(5 + matrix(sort(rnorm(200)), ncol = 4, nrow = 50))
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
#' plot(y.f)}
#' 
#' @export MinT
MinT <- function (fcasts, nodes, groups, residual, covariance = c("shr", "sam"),
  algorithms = c("lu", "cg", "chol"), keep = c("gts", "all", "bottom"))
{
  alg <- match.arg(algorithms)
  keep <- match.arg(keep)
  covar <- match.arg(covariance)
  res <- residual
  fcasts <- stats::as.ts(fcasts)
  tspx <- stats::tsp(fcasts)
  cnames <- colnames(fcasts)

  if(missing(residual))
  {
    stop("MinT needs insample residuals.", call. = FALSE)
  }
  if(covar=="sam")
  {
    n <- nrow(res)
    w.1 <- crossprod(res) / n
    if(is.positive.definite(w.1)==FALSE)
    {
      stop("MinT needs covariance matrix to be positive definite.", call. = FALSE)
    }
  }else{
    tar <- lowerD(res)
    shrink <- shrink.estim(res, tar)
    w.1 <- shrink[[1]]
    lambda <- shrink[[2]]
    if(is.positive.definite(w.1)==FALSE)
    {
      stop("MinT needs covariance matrix to be positive definite.", call. = FALSE)
    }
  }

  if (missing(groups)) { # hts class
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
      allf <- CHOL(fcasts = fcasts, S = smat, weights = w.1)
      }
    else {
      smat <- SmatrixM(gmat)
        if (!is.null(w.1)) {
          weights <-  methods::as(w.1, "sparseMatrix")
        }
        if (alg == "lu") {
          allf <- LU(fcasts = fcasts, S = smat, weights = weights)
        }
        else if (alg == "cg") {
          allf <- CG(fcasts = fcasts, S = smat, weights = weights)
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
  else if (missing(nodes)) {
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
      allf <- CHOL(fcasts = fcasts, S = smat, weights = weights)
    }
    else {
      smat <- SmatrixM(gmat)
      if (!is.null(w.1)) {
        weights <-  methods::as(w.1, "sparseMatrix")
      }
      if (alg == "lu") {
        allf <- LU(fcasts = fcasts, S = smat, weights = weights)
      }
      else if (alg == "cg") {
        allf <- CG(fcasts = fcasts, S = smat, weights = weights)
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
  return(out)
}



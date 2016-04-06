
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
    stop("The data matrix must be numeric!")
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
    stop("MinT needs insample residuals.")
  }
  if(covar=="sam")
  {
    n <- nrow(res)
    w.1 <- crossprod(res) / n
    if(is.positive.definite(w.1)==FALSE)
    {
      stop("MinT needs covariance matrix to be positive definite.")
    }
  }else{
    tar <- lowerD(res)
    shrink <- shrink.estim(res, tar)
    w.1 <- shrink[[1]]
    lambda <- shrink[[2]]
    if(is.positive.definite(w.1)==FALSE)
    {
      stop("MinT needs covariance matrix to be positive definite.")
    }
  }

  if (missing(groups)) { # hts class
    totalts <- sum(Mnodes(nodes))
    if (!is.matrix(fcasts)) {
      fcasts <- t(fcasts)
    }
    h <- nrow(fcasts)
    if (ncol(fcasts) != totalts) {
      stop("Argument fcasts requires all the forecasts.")
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
      stop("Argument fcasts requires all the forecasts.")
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



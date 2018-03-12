#' Optimally combine forecasts from a hierarchical or grouped time series
#' 
#' Using the method of Hyndman et al. (2011), this function optimally combines
#' the forecasts at all levels of a hierarchical time series. The
#' \code{\link{forecast.gts}} calls this function when the \code{comb} method
#' is selected.
#' 
#' 
#' @param fcasts Matrix of forecasts for all levels of the hierarchical time
#' series. Each row represents one forecast horizon and each column represents
#' one time series from the hierarchy.
#' @param nodes If the object class is \code{hts}, a list contains the number
#' of child nodes referring to \code{hts}.
#' @param groups If the object class is \code{gts}, a gmatrix is required,
#' which is the same as \code{groups} in the function \code{gts}.
#' @param weights A numeric vector. The default is \code{NULL} which means that
#' ordinary least squares is implemented.
#' @param algorithms An algorithm to be used for computing reconciled
#' forecasts. See \code{\link{forecast.gts}} for details.
#' @param keep Return a \code{gts} object or the the reconciled forecasts at
#' the bottom level.
#' @return Return the reconciled \code{gts} object or forecasts at the bottom
#' level.
#' @author Alan Lee, Rob J Hyndman and Earo Wang
#' @seealso \code{\link[hts]{hts}}, \code{\link[hts]{forecast.gts}}
#' @references R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang
#' (2011) Optimal combination forecasts for hierarchical time series.
#' \emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
#' \url{http://robjhyndman.com/papers/hierarchical/}
#' 
#' Hyndman, R. J., Lee, A., & Wang, E. (2014). Fast computation of reconciled
#' forecasts for hierarchical and grouped time series. \emph{Working paper
#' 17/14, Department of Econometrics & Business Statistics, Monash University.}
#' \url{http://robjhyndman.com/working-papers/hgts/}
#' @keywords ts
#' @examples
#' 
#' # hts example
#' \dontrun{h <- 12
#' ally <- aggts(htseg1)
#' allf <- matrix(NA, nrow = h, ncol = ncol(ally))
#' for(i in 1:ncol(ally))
#' 	allf[,i] <- forecast(auto.arima(ally[,i]), h = h)$mean
#' allf <- ts(allf, start = 51)
#' y.f <- combinef(allf, get_nodes(htseg1), weights = NULL, keep = "gts", algorithms = "lu")
#' plot(y.f)}
#' 
#' # gts example
#' \dontrun{abc <- ts(5 + matrix(sort(rnorm(200)), ncol = 4, nrow = 50))
#' g <- rbind(c(1,1,2,2), c(1,2,1,2))
#' y <- gts(abc, groups = g)
#' h <- 12
#' ally <- aggts(y)
#' allf <- matrix(NA,nrow = h,ncol = ncol(ally))
#' for(i in 1:ncol(ally))
#'   allf[,i] <- forecast(auto.arima(ally[,i]),h = h)$mean
#' allf <- ts(allf, start = 51)
#' y.f <- combinef(allf, groups = get_groups(y), keep ="gts", algorithms = "lu")
#' plot(y.f)}
#' 
#' @export combinef
combinef <- function(fcasts, nodes, groups, weights = NULL,
                     algorithms = c("lu", "cg", "chol", "recursive", "slm"),
                     keep = c("gts", "all", "bottom")) {
  # Construct optimal combination forecasts
  #
  # Args:
  #   fcasts: all hts/gts forecasts
  #   nodes: nodes for hts
  #   groups: gts
  #   weights: users need to specify the weights
  #   algorithms: different algorithms to obtain reconciled forecasts
  #   keep: choose to return a gts object/all ts/bottom time series
  #
  # Return:
  #   Optimal forecasts
  alg <- match.arg(algorithms)
  keep <- match.arg(keep)
  fcasts <- stats::as.ts(fcasts)
  tspx <- stats::tsp(fcasts)
  cnames <- colnames(fcasts)
  if (missing(groups)) { # hts class
    if (alg == "slm") {
      stop("The slm algorithm does not support an hts object.", call. = FALSE)
    }
    totalts <- sum(Mnodes(nodes))
    if (!is.matrix(fcasts)) {
      fcasts <- t(fcasts)
    }
    h <- nrow(fcasts)
    if (ncol(fcasts) != totalts) {
      stop("Argument fcasts requires all the forecasts.", call. = FALSE)
    }
    if (alg == "recursive") { # only nodes to be needed
      # CombineH only returns bottom time series
      if(is.null(weights)) {
        bf <- CombineH(fcasts, nodes)  # w/o weights
      } else {
        bf <- CombineHw(fcasts, nodes, weights)  # with weights
      }
    } else {
      # Other algorithms return all time series
      gmat <- GmatrixH(nodes)
      fcasts <- t(fcasts)
      if (alg == "chol") {
        smat <- Smatrix(gmat)
        if (!is.null(weights)) {
          weights <-  methods::as(1/weights, "matrix.diag.csr")
        }
        allf <- CHOL(fcasts = fcasts, S = smat, weights = weights)
      } else {
        smat <- SmatrixM(gmat)
        if (!is.null(weights)) {
          seqts <- 1:totalts
          weights <- sparseMatrix(i = seqts, j = seqts, x = 1/weights)
        }
        if (alg == "lu") {
          allf <- LU(fcasts = fcasts, S = smat, weights = weights)
        } else if (alg == "cg") {
          allf <- CG(fcasts = fcasts, S = smat, weights = weights)
        }
      }
    }

    if (keep == "all") {
      if (alg == "recursive") {
        gmat <- GmatrixH(nodes)
        levels <- 1L:nrow(gmat)
        # A function to aggregate the bts
        if (h == 1 && !is.null(weights)) {
          rSum <- function(x) rowsum(as.matrix(bf), gmat[x, ], reorder = FALSE,
                                     na.rm = TRUE)
        } else {
          rSum <- function(x) rowsum(t(bf), gmat[x, ], reorder = FALSE,
                                     na.rm = TRUE)
        }
        ally <- lapply(levels, rSum)
        # Convert lists to matrices
        out <- matrix(unlist(sapply(ally, t)), nrow = h)
      } else {
        out <- t(allf)
      }
    } else {
      if (alg != "recursive") {
        bottom <- totalts - (ncol(smat):1L) + 1L
        bf <- t(allf[bottom, ])
      }
      if (keep == "gts") {
        bf <- ts(bf, start = tspx[1L], frequency = tspx[3L])
        out <- suppressMessages(hts(bf, nodes = nodes))
      } else {
        out <- bf
      }
    }
  } else if (missing(nodes)) {  # gts class
    if (alg == "recursive") {
      stop("The recursive algorithm does not support a gts object.", call. = FALSE)
    }
    # To call Smatrix() properly
    rownames(groups) <- NULL
    gmat <- GmatrixG(groups)
    totalts <- sum(Mlevel(gmat))
    if (ncol(fcasts) != totalts) {
      stop("Argument fcasts requires all the forecasts.", call. = FALSE)
    }
    fcasts <- t(fcasts)
    if (alg == "chol") {
      smat <- Smatrix(gmat)
      if (!is.null(weights)) {
        weights <-  methods::as(1/weights, "matrix.diag.csr")
      }
      allf <- CHOL(fcasts = fcasts, S = smat, weights = weights)
    } else if (alg == "slm") {
      smat <- Smatrix(gmat)
      allf <- SLM(fcasts = fcasts, S = smat, weights = weights)
    } else {
      smat <- SmatrixM(gmat)
      if (!is.null(weights)) {
        seqts <- 1:totalts
        weights <- sparseMatrix(i = seqts, j = seqts, x = 1/weights)
      }
      if (alg == "lu") {
        allf <- LU(fcasts = fcasts, S = smat, weights = weights)
      } else if (alg == "cg") {
        allf <- CG(fcasts = fcasts, S = smat, weights = weights)
      }
    }

    if (keep == "all") {
      out <- t(allf)
    } else {
      bottom <- totalts - (ncol(smat):1L) + 1L
      bf <- t(allf[bottom, ])
      if (keep == "gts") {
        colnames(bf) <- cnames[bottom]
        bf <- ts(bf, start = tspx[1L], frequency = tspx[3L])
        out <- suppressMessages(gts(bf, groups = groups))
      } else {
        out <- bf
      }
    }
  }
  return(out)
}

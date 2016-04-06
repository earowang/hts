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
      stop("The slm algorithm does not support an hts object.")
    }
    totalts <- sum(Mnodes(nodes))
    if (!is.matrix(fcasts)) {
      fcasts <- t(fcasts)
    }
    h <- nrow(fcasts)
    if (ncol(fcasts) != totalts) {
      stop("Argument fcasts requires all the forecasts.")
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
      stop("The recursive algorithm does not support a gts object.")
    }
    # To call Smatrix() properly
    rownames(groups) <- NULL
    gmat <- GmatrixG(groups)
    totalts <- sum(Mlevel(gmat))
    if (ncol(fcasts) != totalts) {
      stop("Argument fcasts requires all the forecasts.")
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

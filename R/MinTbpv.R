# helper function to block principal pivoting algorithm
# can only be used with MinT (sample or shrinkage)
# Author: Shanika Wickramasuriya
# Paper: Optimal non-negative forecast reconciliation

# Arguments
# fcasts: a vector of h-steps-ahead forecasts for all levels of the hierarchical time series. 
# smat: updated original s-matrix (based on the active set constraints)
# vmat: updated covariance matrix to be used.
# alg: algorithm such as "lu", "chol" or "cg"

MinTm <- function(fcasts, smat, vmat, alg) 
{
  totalts <- nrow(smat)
  if (!is.matrix(fcasts)) {
    fcasts <- t(fcasts)
  }
  if (ncol(fcasts) != totalts) {
    stop("Argument fcasts requires all the forecasts.")
  }
  fcasts <- t(fcasts)
  if (alg == "chol") {
    allf <- CHOL(fcasts = fcasts, S = smat, weights = vmat, allow.changes = TRUE)
  } else if (alg == "lu") {
    allf <- LU(fcasts = fcasts, S = smat, weights = vmat, allow.changes = TRUE)
  } else {
    allf <- CG(fcasts = fcasts, S = smat, weights = vmat, allow.changes = TRUE)
  }
  return(allf)
}

# Arguments
# fcasts: a vector of h-steps-ahead forecasts for all levels of the hierarchical time series. 
# nodes: Hierarchical structure
# groups: Grouping structure
# res: in-sample residuals of the base forecasts
# covar: covariance matrix (sam vs shr)
# alg: algorithm such as "lu", "chol" or "cg"
# control.nn: A list of control parameters to be used in the non-negative algorithm. 
# This includes ptype (fixed or random), par, and gtol (tolerance of the convergence criteria)

MinTbpv <- function(fcasts, nodes = NULL, groups = NULL, res, covar,
                    alg, control.nn  = list())
{
  # ptype <- match.arg(ptype)
  con <- list(ptype = "fixed", pbar = 10, gtol = sqrt(.Machine$double.eps))
  nmsC <- names(con)
  con[(namc <- names(control.nn))] <- control.nn
  if (length(noNms <- namc[!namc %in% nmsC])) 
    warning("unknown names in control.nn: ", paste(noNms, 
                                                   collapse = ", "))
  
  if (is.null(groups)) { # hts class
    gmat <- GmatrixH(nodes)
    if (alg == "chol") {
      smat <- Smatrix(gmat)
    } else if (alg == "lu" || alg == "cg") {
      smat <- SmatrixM(gmat) 
    }
    totalts <- nrow(smat)
    if (!is.matrix(fcasts)) {
      fcasts <- t(fcasts)
    }
    if (ncol(fcasts) != totalts) {
      stop("Argument fcasts requires all the forecasts.")
    }
    nb <- ncol(smat) # number of bottom level series
    nt <- nrow(smat) # total no of series
    nxb <- nt - nb # no of series, except the bottom level
    ifcasts <- MinT(fcasts = fcasts, nodes = nodes, groups = groups, residual = res, covariance = covar, algorithms = alg, keep = "all") 
    b <- ifcasts[(nxb + 1):nt] # initial solution-bottom level
  } else if (is.null(nodes)) { # gts class
    rownames(groups) <- NULL
    gmat <- GmatrixG(groups)
    if (alg == "chol") {
      smat <- Smatrix(gmat)
    } else if (alg == "lu" || alg == "cg") {
      smat <- SmatrixM(gmat)
    } 
    totalts <- nrow(smat)
    if (!is.matrix(fcasts)) {
      fcasts <- t(fcasts)
    }
    if (ncol(fcasts) != totalts) {
      stop("Argument fcasts requires all the forecasts.")
    }
    nb <- ncol(smat) # number of bottom level series
    nt <- nrow(smat) # total no of series
    nxb <- nt - nb # no of series, except the bottom level
    ifcasts <- MinT(fcasts = fcasts, nodes =  nodes, groups = groups, residual = res, algorithms = alg, keep = "all") # initial solution
    b <- ifcasts[(nxb + 1):nt] # initial solution-bottom level
  }
  
  if (all(b > -con$gtol)) {
    bf <- t(b)
    return(bf)
  } else {
    b <- numeric(nb)
    if (covar == "sam") {
      w.1 <- crossprod(res) / nt
      if (is.positive.definite(w.1) == FALSE) {
        stop("MinT needs covariance matrix to be positive definite", call. = FALSE)
      }
    } else if (covar == "shr") {
      tar <- lowerD(res)
      shrink <- shrink.estim(res, tar)
      w.1 <- shrink[[1]]
      if (is.positive.definite(w.1) == FALSE) {
        stop("MinT needs covariance matrix to be positive definite", call. = FALSE)
      }
    }
    if (alg == "chol") {
      w.1 <- as.matrix.csr(w.1)
    } else if (alg == "lu" || alg == "cg") {
      w.1 <- methods::as(w.1, "sparseMatrix")
    }
    w <- solve(w.1)
    z <- t(smat) %*% w # z %*% t(z) = t(s) %*% L %*% s
    wy <- w %*% t(fcasts)
    y <- t(smat) %*% wy # t(s) %*% L %*% yhat
    sb <- smat %*% as.matrix(b) 
    
    tol <- sqrt(.Machine$double.eps)
    grad <- as.matrix(z %*% sb) - y
    maxp <- con$pbar
    ninf <- nb + 1
    
    if (con$ptype == "fixed") {
      alpha <- 1:nb
    } else if (con$ptype == "random") {
      alpha <- sample(1:nb, nb, replace = FALSE)
    }
    
    fset <- numeric(0)
    gset <- 1:nb
    
    bf <- b[fset]
    gradg <- grad[gset]
    
    # To avoid nondegenerate problem (as done by Jason Cantarella)
    idxf <- abs(bf) < tol
    idxg <- abs(gradg) < tol
    bf[idxf] <- 0L
    gradg[idxg] <- 0L
    
    # convergence criteria
    converged <- all(bf > -con$gtol) & all(gradg > -con$gtol)
    
    while (!converged) {
      i1 <- fset[which(bf < -tol)] 
      i2 <- gset[which(gradg < -tol)]
      ivec <- union(i1, i2)
      
      if (length(ivec) < ninf) {
        ninf <- length(ivec)
        maxp <- con$pbar
      } else if (maxp >= 1) {
        maxp <- maxp - 1
      } else {
        if (con$ptype == "fixed") {
          cat("You are entering a slow zone! It might take some time to converge! \n")
          r <- max(ivec)
        } else if (con$ptype == "random") {
          cat("You are entering a slow zone! It might take some time to converge! \n")
          r <- alpha[max(which(alpha %in% ivec))]
        }
        if (is.element(r, i1)) {
          i1 <- r
          i2 <- numeric(0)
        } else {
          i1 <- numeric(0)
          i2 <- r
        }
      }
      
      # updating f and g
      fset <- union(fset[!fset %in% i1], i2)
      gset <- union(gset[!gset %in% i2], i1)
      
      if (length(gset) == 0) {
        tmp <- MinTm(fcasts = fcasts, smat = smat, vmat = w.1, alg = alg)
        allf <- as.numeric(tmp)
      } else {
        usmat <- smat[, -gset]
        tmp <- MinTm(fcasts = fcasts, smat = usmat, vmat = w.1, alg = alg)
        allf <- as.numeric(tmp)
      }
      
      b <- tail(allf, nb)
      sb <- smat %*% as.matrix(b)
      grad <- as.matrix(z %*% sb) - y
      bf <- b[fset]
      gradg <- grad[gset]
      
      # To avoid nondegenerate problem (as done by Jason Cantarella)
      idxf <- abs(bf) < tol
      idxg <- abs(gradg) < tol
      bf[idxf] <- 0L
      gradg[idxg] <- 0L
      converged <- all(bf > -con$gtol) & all(gradg > -con$gtol)
    }

    bf <- t(b)
  }
  return(bf)
}


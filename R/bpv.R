# block principal pivoting algorithm
# can only be used with OLS, WLS (any positive weights)
# Author: Shanika Wickramasuriya
# Paper: Optimal non-negative forecast reconciliation

# Arguments
# fcasts: a vector of h-steps-ahead forecasts for all levels of the hierarchical time series. 
# nodes: Hierarchical structure
# groups: Grouping structure
# weights: weights to be used in OLS or WLS
# control.nn: A list of control parameters to be used in the non-negative algorithm. 
# This includes ptype (fixed or random), par, and gtol (tolerance of the convergence criteria)

bpv <- function(fcasts, nodes = NULL, groups = NULL, weights = NULL, alg, control.nn = list())
{
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
    ifcasts <- combinef(fcasts = fcasts, nodes = nodes, weights = weights, algorithms = alg, keep = "all") 
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
    ifcasts <- combinef(fcasts = fcasts, groups = groups, weights = weights, algorithms = alg, keep = "all") # initial solution
    b <- ifcasts[(nxb + 1):nt] # initial solution-bottom level
  }
  
  if (alg == "chol") {
    if (!is.null(weights)) {
      sqwt <- sqrt(weights)
      w <- methods::as(1/sqwt, "matrix.diag.csr") # w^2 = L
      z <- t(smat) %*% w # Sparse. z %*% t(z) = t(s) %*% L %*% s
      wy <- sqwt * t(fcasts)
      y <- z %*% wy # t(s) %*% L %*% yhat
    } else {
      z <- t(smat) # Sparse. z %*% t(z) = t(s) %*% s
      y <- z %*% t(fcasts) # t(s) %*% yhat
    }
  } else if (alg == "lu" || alg == "cg") {
    if (!is.null(weights))
    {
      sqwt <- sqrt(weights)      
      seqts <- seq(nrow(smat))
      w <- sparseMatrix(i = seqts, j = seqts, x = sqwt) #w^2 = L
      z <- t(smat) %*% w # Sparse. z %*% t(z) = t(s) %*% L %*% s
      wy <- sqwt * t(fcasts)
      y <- z %*% wy # t(s) %*% L %*% yhat
    } else {
      z <- t(smat) # Sparse. z %*% t(z) = t(s) %*% s
      y <- z %*% t(fcasts) # t(s) %*% yhat
    }
  }
  
  tol <- sqrt(.Machine$double.eps)
  tzb <- t(z) %*% as.matrix(b)
  grad <- as.matrix(z %*% tzb - y)
  maxp <- con$pbar
  ninf <- nb + 1
  
  if (con$ptype == "fixed") {
    alpha <- 1:nb
  } else if (con$ptype == "random") {
    alpha <- sample(1:nb, nb, replace = FALSE)
  }
  
  fset <- 1:nb
  gset <- numeric(0)
  
  bf <- b[fset]
  gradg <- grad[gset]
  
  # To avoid nondegenerate problem (as done by Jason Cantarella)
  idxf <- abs(bf) < tol
  idxg <- abs(gradg) < tol
  bf[idxf] <- 0L
  gradg[idxg] <- 0L
  
  # convergence criteria
  converged <- all(bf > -con$gtol) & all(gradg > -con$gtol)
  
  if (converged) {
    bf <- t(b)
    return(bf)
  } else {
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
      
      if (is.null(groups)) { # class hts
        allf <- numeric(nt)
        uwts <- weights
        
        if (length(gset) == 0) {
          ufcasts <- fcasts
          tmp <- combinefm(fcasts = ufcasts, smat = smat, weights = uwts, alg = alg)
          allf <- as.numeric(tmp)
        } else {
          usmat <- smat[, -gset]
          zidx <- setdiff(1:nt, unique(summary(usmat)$i)) # identifying rows with zeros
          if (length(zidx) != 0)
          {
            idxR <- unique(c(gset + nxb, zidx)) # series with zeros in the summing matrix and active constraints
            ufcasts <- fcasts[-idxR]
            if (!is.null(weights)) {
              uwts <- uwts[-idxR]
            } 
            usmat <- usmat[-idxR, ]
            tmp <- combinefm(fcasts = ufcasts, smat = usmat, weights = uwts, alg = alg)
            allf[sort(setdiff(c(1:nxb, (fset + nxb)), zidx))] <- as.numeric(tmp)
            
          } else {
            idxR <- gset + nxb
            ufcasts <- fcasts[-idxR]
            if (!is.null(weights)) {
              uwts <- uwts[-idxR]              
            }
            tmp <- combinefm(fcasts = ufcasts, smat = usmat, weights = uwts, alg = alg)
            allf[c(1:nxb, (fset + nxb))] <- as.numeric(tmp)
          }
        }
      } else if (is.null(nodes)) { # class gts
        allf <- numeric(nt)
        uwts <- weights
        
        if (length(gset) == 0) {
          ufcasts <- fcasts
          tmp <- combinefm(fcasts = ufcasts, smat = smat, weights = uwts, alg = alg)
          allf <- as.numeric(tmp)
        } else {
          usmat <- smat[, -gset]
          zidx <- setdiff(1:nt, unique(summary(usmat)$i)) # identifying rows with zeros
          if (length(zidx) != 0)
          {
            idxR <- unique(c(gset + nxb, zidx)) # series with zeros in the summing matrix and active constraints
            ufcasts <- fcasts[-idxR]
            if (!is.null(weights)) {
              uwts <- uwts[-idxR]
            } 
            usmat <- usmat[-idxR, ]
            tmp <- combinefm(fcasts = ufcasts, smat = usmat, weights = uwts, alg = alg)
            allf[sort(setdiff(c(1:nxb, (fset + nxb)), zidx))] <- as.numeric(tmp)
            
          } else {
            idxR <- gset + nxb
            ufcasts <- fcasts[-idxR]
            if (!is.null(weights)) {
              uwts <- uwts[-idxR]              
            }
            tmp <- combinefm(fcasts = ufcasts, smat = usmat, weights = uwts, alg = alg)
            allf[c(1:nxb, (fset + nxb))] <- as.numeric(tmp)
          }
        }
      }
      
      b <- tail(allf, nb)
      tzb <- t(z) %*% as.matrix(b)
      grad <- as.matrix(z %*% tzb) - y
      bf <- b[fset]
      gradg <- grad[gset]
      
      # To avoid nondegenerate problem (as done by Jason Cantarella)
      idxf <- abs(bf) < tol
      idxg <- abs(gradg) < tol
      bf[idxf] <- 0L
      gradg[idxg] <- 0L
      converged <- all(bf > -con$gtol) & all(gradg > -con$gtol)
    }
  } 
  
  bf <- t(b)
  return(bf)
}


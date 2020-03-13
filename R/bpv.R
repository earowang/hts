bpv <- function(fcasts, nodes, groups, weights = NULL, alg,
                ptype = c("fixed", "random"), pbar = 10, gtol = sqrt(.Machine$double.eps))
{
  if (missing(groups)) { # hts class
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
    lnodes <- length(nodes)
    nb <- length(nodes[[lnodes]]) # number of bottom level series
    nt <- nrow(smat) # total no of series
    nxb <- nt - nb # no of series, except the bottom level
    ifcasts <- combinef(fcasts = fcasts, nodes = nodes, weights = weights, algorithms = alg, keep = "all") 
    b <- ifcasts[(nxb + 1):nt] # initial solution-bottom level
  } else if (missing(nodes)) { # gts class
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
    lgroups <- nrow(groups)
    nb <- length(groups[lgroups, ]) # number of bottom level series
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
  grad <- as.matrix(z %*% tzb) - y
  maxp <- pbar
  ninf <- nb + 1
  
  if (ptype == "fixed") {
    alpha <- 1:nb
  } else if (ptype == "random") {
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
  converged <- all(bf > -gtol) & all(gradg > -gtol)
  
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
        maxp <- pbar
      } else if (maxp >= 1) {
        maxp <- maxp - 1
      } else {
        if (ptype == "fixed") {
          cat("You are entering a slow zone! It might take some time to converge! \n")
          r <- max(ivec)
        } else if (ptype == "random") {
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
      
      if (missing(groups)) { # class hts
        useH <- TRUE
        # gset0 <- 1:nbot %in% gset
        # active <- which(gset0 == TRUE) # active indices at the bottom level
        allf <- numeric(nt)
        uwts <- weights
        
        if (length(gset) == 0) {
          ufcasts <- fcasts
          # if (!is.null(weights)) {
          #   weightsnw <- weightsnw
          # }
          tmp <- combinefnw(ufcasts, use = useH, smat = smat, weights = uwts, algorithms = alg)
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
            tmp <- combinefnw(ufcasts, use = useH, smat = usmat, weights = uwts, algorithms = alg)
            allf[sort(setdiff(c(1:nxb, (fset + nxb)), zidx))] <- as.numeric(tmp)
            
          } else {
            idxR <- gset + nxb
            ufcasts <- fcasts[-idxR]
            if (!is.null(weights)) {
              uwts <- uwts[-idxR]              
            }
            tmp <- combinefnw(ufcasts, use = useH, smat = usmat, weights = uwts, algorithms = alg)
            allf[c(1:nxb, (fset + nxb))] <- as.numeric(tmp)
          }
        }
      } else if (missing(nodes)) { # class gts
        useH <- FALSE
        allf <- numeric(nt)
        uwts <- weights
        
        if (length(gset) == 0) {
          ufcasts <- fcasts
          # if (!is.null(weights)) {
          #   weightsnw <- weightsnw
          # }
          tmp <- combinefnw(ufcasts, use = useH, smat = smat, weights = uwts, algorithms = alg)
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
            tmp <- combinefnw(ufcasts, use = useH, smat = usmat, weights = uwts, algorithms = alg)
            allf[sort(setdiff(c(1:nxb, (fset + nxb)), zidx))] <- as.numeric(tmp)
            
          } else {
            idxR <- gset + nxb
            ufcasts <- fcasts[-idxR]
            if (!is.null(weights)) {
              uwts <- uwts[-idxR]              
            }
            tmp <- combinefnw(ufcasts, use = useH, smat = usmat, weights = uwts, algorithms = alg)
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
      converged <- all(bf > -gtol) & all(gradg > -gtol)
    }
  } 
  
  bf <- t(bf)
  return(bf)
}


combinef <- function(fcasts, nodes, groups, weights = NULL, 
                     keep = c("gts", "bottom")) {
  # Construct optimal combination forecasts
  #
  # Args:
  #   fcasts: all hts/gts forecasts
  #   nodes: nodes for hts
  #   groups: gts
  #   weights: users need to specify the weights
  #   keep: choose to return a gts object or bottom time series
  #
  # Return:
  #   Optimal forecasts
  keep <- match.arg(keep)
  fcasts <- as.ts(fcasts)
  tspx <- tsp(fcasts)
  if (missing(groups)) { # hts class
    totalts <- sum(Mnodes(nodes))
    if (!is.matrix(fcasts)) {
      fcasts <- t(fcasts)
    }
    if (ncol(fcasts) != totalts) {
      stop("Argument fcasts requires all the forecasts.")
    }
    if(is.null(weights)) {
      bf <- CombineH(fcasts, nodes)  # w/o weights
    } else {
      bf <- CombineHw(fcasts, nodes, weights)  # with weights
    }

    if (keep == "gts") {
      bf <- ts(bf, start = tspx[1L], frequency = tspx[3L])
      out <- suppressMessages(hts(bf, nodes = nodes))
    } else {
      out <- bf
    }
  } else if (missing(nodes)) {  # gts class
    # To call Smatrix() properly
    rownames(groups) <- NULL
    allgroups <- GmatrixG(groups)
    fcasts <- structure(list(all = fcasts, groups = allgroups), class = "gts")
    totalts <- sum(Mlevel(fcasts$groups))
    if (ncol(fcasts$all) != totalts) {
      stop("Argument fcasts requires all the forecasts.")
    }
    smat <- Smatrix(fcasts)
    if (is.null(weights)) {
      bf <- CombineG(fcasts$all, smat)
    } else {
      bf <- CombineG(fcasts$all, smat, weights)
    }

    if (keep == "gts") {
      bf <- ts(bf, start = tspx[1L], frequency = tspx[3L])
      out <- suppressMessages(gts(bf, groups = groups))
    } else {
      out <- bf
    } 
  }
  return(out)
}

# Combination approach 
# Author: Alan Lee; largely improved by Earo Wang

UpdateC <- function(c.list) {
  k <- length(c.list)
  div <- 1L
  nvec <- numeric(k)
  comb.vec <- NULL
  for (i in 1:k) {
    m <- c.list[[i]][[2L]]
    cc <- c.list[[i]][[1L]]
    nvec[i] <- dim(cc)[1L]
    div <- div - sum(m * (cc %*% m))
    comb.vec <- c(comb.vec, m)
  }
  d <- sum(comb.vec)
  div <- div + d
  sum.nvec <- sum(nvec)
  c.star <- matrix(, sum.nvec, sum.nvec)
  hi <- cumsum(nvec)
  lo <- cumsum(c(1L, nvec[-length(nvec)]))
  for (i in 1:k) {
    cm1 <- as.vector(c.list[[i]][[1L]] %*% c.list[[i]][[2L]])
    row.range <- lo[i]:hi[i]
    for (j in 1:k) {
      cm2 <- as.vector(c.list[[j]][[1L]] %*% c.list[[j]][[2]])
      cinsert <- outer(1 - cm1, 1 - cm2)/div
      if (i == j) {
        cinsert <- c.list[[i]][[1L]] + cinsert
      }
      col.range <- lo[j]:hi[j]
      c.star[row.range, col.range] <- cinsert
    }
  }
  return(list(C = c.star, nvec = comb.vec))
}

CombineH <- function(fcasts, nodes) {
  class(fcasts) <- "matrix" # drop "ts" object to process faster
  # Split fcasts to a list
  levels <- cumsum(Mnodes(nodes))
  l.levels <- length(levels)
  flist <- lapply(2L:l.levels, function(x) {
                    fcasts[, seq(levels[x - 1L] + 1L, levels[x]), drop = FALSE]
                  })
  flist <- c(list(fcasts[, 1L, drop = FALSE]), flist)
  rm(fcasts)

  # Start with the last level
  lenl <- length(levels)
  lenn <- length(nodes)
  last.nodes <- nodes[[lenn]]
  last.len <- length(last.nodes)
  cmat <- lapply(1:last.len, function(x) 
                 list(matrix(1/(last.nodes[x] + 1)), last.nodes[x]))
  idx <- c(0, cumsum(last.nodes))
  smat <- lapply(1L:last.len, function(x) 
            flist[[lenl]][, (idx[x] + 1L):idx[x + 1L], drop = FALSE] 
            + flist[[lenl - 1L]][, x])

  if (lenn == 1L) { # A simple hierarchy with only 2 levels
    cmat <- UpdateC(cmat[1L])$C
    smat <- apply(smat[[1L]], 2, function(x) x + flist[[1L]])
    sums <- rowsum(t(smat), rep(1L:last.len, last.nodes))
    comb <- smat - rep(cmat %*% sums, last.nodes)
  } else { # more than 2 levels
    # Recursively update C matrix from L - 1 to 1
    for (i in 1L:(lenn - 1L)) {
      newn <- nodes[[lenn - i]]
      newl <- length(newn)
      new.cmat <- vector(length = newl, mode = "list")
      new.smat <- vector(length = newl, mode = "list")
      idx <- c(0L, cumsum(newn))
      for (j in 1L:newl) {
        new.cmat[[j]] <- UpdateC(cmat[(idx[j] + 1L):idx[j + 1L]])
        sblock <- smat[(idx[j] + 1L):idx[j + 1L]]
        sblock <- do.call("cbind", sblock)
        new.smat[[j]] <- flist[[lenl - i - 1L]][, j] + sblock
      }
      cmat <- new.cmat
      smat <- new.smat
    }
    cmat <- cmat[[1L]]$C
    comb <- t(apply(smat[[1L]], 1, function(x) {
                    sums <- rowsum(x, rep(1L:last.len, last.nodes))
                    return(x - rep(cmat %*% sums, last.nodes))
                    }))
  }

  colnames(comb) <- NULL
  return(comb)
}


# Combination with weights
UpdateCw <- function(c.list, d1.vec, d0) {
  l.c <- length(c.list)
  comb.vec <- NULL
  nvec <- numeric(l.c)
  div <- d0
  for (i in 1L:l.c) {
    m <- c.list[[i]][[2L]]
    cmat <- c.list[[i]][[1L]]
    d <- d1.vec[m]
    nvec[i] <- length(m)
    div <- div + sum(d) - sum(d * (cmat %*% d))
    comb.vec <- c(comb.vec, m)
  }

  len.comb <- length(comb.vec)
  c.star <- matrix(, nrow = len.comb, ncol = len.comb)
  hi <- cumsum(nvec)
  lo <- cumsum(c(1L, nvec[-length(nvec)]))

  for (i in 1L:l.c) {
    di <- d1.vec[c.list[[i]][[2L]]]
    cd1 <- as.vector(c.list[[i]][[1L]] %*% di)
    row.range <- lo[i]:hi[i]
    for (j in 1L:l.c) {
      col.range <- lo[j]:hi[j]
      dj <- d1.vec[c.list[[j]][[2L]]]
      cd2 <- as.vector(c.list[[j]][[1L]] %*% dj)
      cinsert <- outer(1L - cd1, 1L - cd2)/div
      if (i == j) {
        cinsert <- c.list[[i]][[1L]] + cinsert
      }
      c.star[row.range, col.range] <- cinsert
    }
  }
  return(list(cmat = c.star, m = comb.vec))
}

CombineHw <- function(fcasts, nodes, weights) {
  class(fcasts) <- "matrix" # drop "ts" object to process faster
  # Split fcasts to a list
  levels <- cumsum(Mnodes(nodes))
  l.levels <- length(levels)
  flist <- lapply(2L:l.levels, function(x) {
                    fcasts[, seq(levels[x - 1L] + 1L, levels[x]), drop = FALSE]
                  })
  flist <- c(list(fcasts[, 1L, drop = FALSE]), flist)
  rm(fcasts)
  # Split weights to a list
  wlist <- lapply(2L:l.levels, function(x) {
                    weights[seq(levels[x - 1L] + 1L, levels[x])]
                  })
  wlist <- c(list(weights[1L]), wlist)

  # Start with the last level
  lenl <- length(levels)
  lenn <- length(nodes)
  last.nodes <- nodes[[lenn]]
  last.len <- length(last.nodes)
  lastg <- rep(1L:last.len, last.nodes)
  dlist <- split(1/wlist[[l.levels]], lastg)
  d1vec <- sapply(dlist, sum)
  d0 <- 1/wlist[[l.levels - 1L]]
  cmat <- lapply(1:last.len, function(x) 
                 list(matrix(1L/(d0[x] + sum(dlist[[x]]))), x))
  idx <- c(0, cumsum(last.nodes))
  smat <- lapply(1L:last.len, function(x) {
              yy <- flist[[lenl]][, (idx[x] + 1L):idx[x + 1L], drop = FALSE] *
                    wlist[[lenl]][(idx[x] + 1L):idx[x + 1L]]
              return(yy + flist[[lenl - 1L]][, x] * wlist[[lenl - 1L]][x])
            })

  if (lenn == 1L) { # A simple hierarchy with only 2 levels
    cmat <- UpdateCw(cmat[1L], d1vec, d0)$cmat
    dvec <- unlist(dlist)
    smat <- apply(smat[[1L]], 2, function(x) x + flist[[1L]] * wlist[[1L]])
    sums <- rowsum(t(smat), rep(1L:last.len, last.nodes))
    comb <- (smat - rep(cmat %*% sums, last.nodes)) * dvec
  } else { # more than 2 levels
    # Recursively update C matrix from L - 1 to 1
    for (i in 1L:(lenn - 1L)) {
      d0 <- 1/wlist[[lenl - i - 1L]]
      newn <- nodes[[lenn - i]]
      newl <- length(newn)
      new.cmat <- vector(length = newl, mode = "list")
      new.smat <- vector(length = newl, mode = "list")
      idx <- c(0L, cumsum(newn))
      for (j in 1L:newl) {
        new.cmat[[j]] <- UpdateCw(cmat[(idx[j] + 1L):idx[j + 1L]], d1vec, d0[j])
        sblock <- smat[(idx[j] + 1L):idx[j + 1L]]
        sblock <- do.call("cbind", sblock)
        tmpw <- wlist[[lenl - i - 1L]][j]
        new.smat[[j]] <- flist[[lenl - i - 1L]][, j] * tmpw + sblock
      }
      cmat <- new.cmat
      smat <- new.smat
      }
      cmat <- cmat[[1L]]$cmat
      dvec <- unlist(dlist)
      comb <- t(apply(smat[[1L]], 1, function(x) {
                      sums <- rowsum(x * dvec, rep(1L:last.len, last.nodes))
                      return((x - rep(cmat %*% sums, last.nodes)) * dvec)
                      }))
    }

  colnames(comb) <- NULL
  return(comb)
}

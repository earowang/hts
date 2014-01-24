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
# Author: Alan Lee

BasicC <- function(n) { 
  k <- length(n)
  c.list <- vector(length = k, mode = "list")
  for (i in 1L:k) {
    c.list[[i]] <- list(matrix(1/(n[i] + 1)), n[i])
  }
  return(c.list)
}

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
  c.star <- matrix(, sum(nvec), sum(nvec))
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

CombineH <- function(fcasts, nList) {
  nList <- c(1L, nList)
  l <- length(nList)
  n <- sum(unlist(nList))
  levels <- rep(1L, n)

  node <- 2L
  for (l in 2L:l) {
    for (i in 1L:length(nList[[l]])) {
      for (j in 1:nList[[l]][i]) {
        levels[node] <- l
        node <- node + 1L
      }
    }
  }
  c.list <- BasicC(nList[[l]])
  newl <- length(nList[[l]])
  s.list <- vector(length = nrow(fcasts), mode = "list")
  for (h in 1:nrow(fcasts)) {
    y <- fcasts[h, ]
    s.list[[h]] <- vector(length = newl, mode = "list")
    m <- c(0L, cumsum(nList[[l]]))
    for (i in 1L:newl) {
      yy <- y[levels == l][(m[i] + 1L):m[i + 1L]]
      s.list[[h]][[i]] <- yy + y[levels == l - 1L][i]
    }
  }
  new.s.list <- vector(length = nrow(fcasts), mode = "list")
  for (i in 1L:(l - 2L)) {
    newl <- length(nList[[l - i]])
    new.c.list <- vector(length = newl, mode = "list")
    new.s.list <- vector(length = newl, mode = "list")
    m <- c(0L, cumsum(nList[[l - i]]))
    for (h in 1:nrow(fcasts)) {
      y <- fcasts[h, ]
      for (j in 1L:newl) {
        new.c.list[[j]] <- UpdateC(c.list[(m[j] + 1L):m[j + 1L]])
        y0 <- y[levels == l - i - 1L][j]
        new.s.list[[j]] <- y0 + unlist(s.list[[h]][(m[j] + 1L):m[j + 1L]])
      }
      s.list[[h]] <- new.s.list
    }
    c.list <- new.c.list
  }
  cc <- c.list[[1L]]$C
  n <- nList[[l]]
  comb <- matrix(, nrow = nrow(fcasts), ncol = sum(n))
  for (h in 1L:nrow(fcasts)) {
    sty <- unlist(s.list[[h]])
    sums <- rowsum(sty, rep(1L:length(n), n))
    comb[h, ] <- sty - rep(cc %*% sums, n)
  }
  return(comb)
}

BasicCw <- function(d0, d.list) {
  l.d0 <- length(d0)
  c.list <- vector(length = l.d0, mode = "list")
  for (i in 1L:l.d0) {
    c0 <- 1L/(d0[i] + sum(d.list[[i]]))
    c.list[[i]] <- list(cmat = matrix(c0, 1L, 1L), m = i)
  }
  return(c.list)
}

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

  c.star <- matrix(, nrow = length(comb.vec), ncol = length(comb.vec))
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

SumSplit <- function(x, n) {
  gr <- rep(1L:length(n), n)
  out <- rowsum(x, gr)
  return(out)
}

CombineHw <- function(fcasts, nodes, weights) {
  H <- nrow(fcasts)
  nodes <- c(1L, nodes)
  # weights <- sqrt(weights)
  l.nodes <- length(nodes)
  n.nodes <- sum(nodes[[l.nodes]])
  adj.fcasts <- matrix(, nrow = H, ncol = n.nodes)
  n <- sum(unlist(nodes))
  levels <- rep(1L, n)
  labels <- 1L:n

  node <- 2L
  for (l in 2L:l.nodes) {
    for (i in 1L:length(nodes[[l]])) {
      for (j in 1L:nodes[[l]][i]) {
        levels[node] <- l
        node <- node + 1L
      }
    }
  }

  k <- length(nodes[[l.nodes]])
  d.list <- vector(length = k, mode = "list")
  m <- c(0L, cumsum(nodes[[l.nodes]]))
  for (i in 1L:k) {
    d.list[[i]] <- 1/weights[levels == l.nodes][(m[i] + 1L):m[i + 1L]]
  }

  d1.vec <- unlist(lapply(d.list, sum))
  d0 <- 1/weights[levels == l.nodes - 1L]
  c.list <- BasicCw(d0, d.list)

  newl <- length(nodes[[l.nodes]])

  sw.list <- vector(length = H, mode = "list")

  for (h in 1L:H) {
    fcast <- fcasts[h, ]
    sw.list[[h]] <- vector(length = newl, mode = "list")
    m <- c(0L, cumsum(nodes[[l.nodes]]))
    for (i in 1L:newl) {
      yy <- fcast[levels == l.nodes][(m[i] + 1L):m[i + 1L]] * 
            weights[levels == l.nodes][(m[i] + 1L):m[i + 1L]]
      sw.list[[h]][[i]] <- yy + fcast[levels == l.nodes - 1L][i] *
            weights[levels == l.nodes - 1L][i]  
    } 
  }
    
  new.s.list <- vector(length = H, mode = "list")

  for (i in 1L:(l.nodes - 2L)) {
    newl <- length(nodes[[l.nodes - i]])
    new.c.list <- vector(length = newl, mode = "list")
    new.s.list <- vector(length = newl, mode = "list")
    m <- c(0L, cumsum(nodes[[l.nodes - i]]))
    d0 <- 1/weights[levels == l.nodes - i - 1L]
    for (h in 1L:H) {
      fcast <- fcasts[h, ]
      for (j in 1L:newl) {
        new.c.list[[j]] <- UpdateCw(c.list[(m[j] + 1L):m[j + 1L]], d1.vec, d0[j])
        y0 <- fcast[levels == l.nodes - i - 1L][j]
        w0 <- weights[levels == l.nodes - i - 1L][j]
        new.s.list[[j]] <- y0 * w0 + unlist(sw.list[[h]][(m[j] + 1L):m[j + 1L]])
      }
      sw.list[[h]] <- new.s.list
    }
    c.list <- new.c.list
  }

  cmat <- c.list[[1L]]$cmat
  dvec <- unlist(d.list)
  for (h in 1L:H) {
    stwy <- unlist(sw.list[[h]])
    tvec <- SumSplit(stwy * dvec, nodes[[l.nodes]])
    adj.fcast <- (stwy - rep(cmat %*% tvec, nodes[[l.nodes]])) * dvec
    adj.fcasts[h, ] <- adj.fcast
  }
  return(adj.fcasts)  # Only return fcasts at the bottom level
}

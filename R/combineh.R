combineh <- function(fcasts, nodes, weights = FALSE, wvec) {
  # Construct optimal combination forecasts
  #
  # Args:
  #   fcasts: hts forecasts
  #   nodes: nodes for hts
  #   weights: weighting vector 
  #   wvec: if weights = TRUE, users need to specify the weights
  #
  # Return:
  #   Optimal forcasts
  if(weights) {
    bf <- Combinehw(fcasts, nodes, wvec)
  } else {
    bf <- Combineh(fcasts, nodes)
  }
  return(bf)
}

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

Combineh <- function(fcasts, nList) {
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
    sums <- tapply(sty, rep(1L:length(n), n), sum)
    comb[h, ] <- sty - rep(cc %*% sums, n)
  }
  return(comb)
}

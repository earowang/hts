# Combination approach w weights
# Author: Alan Lee

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
  div <- d0
  comb.vec <- NULL
  nvec <- numeric(l.c)
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
  out <- tapply(x, gr, sum)
  return(out)
}

Combinefw <- function(fcasts, nodes, weights) {
  H <- nrow(fcasts)
  nodes <- c(1L, nodes)
  l.nodes <- length(nodes)
  n.nodes <- sum(nodes[[l.nodes]])
  all.c <- array(, c(sum(nodes[[l.nodes - 1L]]), sum(nodes[[l.nodes - 1L]]), H))
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

  newl <- length(nodes[[l.nodes]])
  l.list <- vector(length = newl, mode = "list")
  m <- c(0L, cumsum(nodes[[l.nodes]]))
  for (j in 1L:newl) {
    l.list[[j]] <- c(labels[levels == l.nodes - 1L][j], 
                     labels[levels == l.nodes][(m[j] + 1L):m[j + 1L]])
  }

  for (i in 1L:(l.nodes - 2L)) {
    newl <- length(nodes[[l.nodes - i]])
    new.l.list <- vector(length = newl, mode = "list")
    m <- c(0, cumsum(nodes[[l.nodes - i]]))
    for (j in 1L:newl) {
      new.l.list[[j]] <- c(labels[levels == l.nodes - i - 1L][j],
                           unlist(l.list[(m[j] + 1L):m[j + 1L]]))
    }
    l.list <- new.l.list
  }

  levels <- levels[unlist(l.list)]

  for (h in 1L:H) {
    fcast <- fcasts[h, ]
    w <- weights[h, ]
    k <- length(nodes[[l.nodes]])
    d.list <- vector(length = k, mode = "list")
    m <- c(0L, cumsum(nodes[[l.nodes]]))
    for (i in 1L:k) {
      d.list[[i]] <- 1/w[levels == l.nodes][(m[i] + 1L):m[i + 1L]]
    }

    d1.vec <- unlist(lapply(d.list, sum))
    d0 <- 1/w[levels == l.nodes - 1L]
    c.list <- BasicCw(d0, d.list)

    newl <- length(nodes[[l.nodes]])

    sw.list <- vector(length =newl, mode = "list")
    m <- c(0L, cumsum(nodes[[l.nodes]]))
    for (i in 1L:newl) {
      yy <- fcast[levels == l.nodes][(m[i] + 1L):m[i + 1L]] * 
            w[levels == l.nodes][(m[i] + 1L):m[i + 1L]]
      sw.list[[i]] <- yy + fcast[levels == l.nodes - 1L][i] *
            w[levels == l.nodes - 1L][i]  
    } 
    
    new.s.list <- vector(length = length(fcast), mode = "list")

    for (i in 1L:(l.nodes - 2L)) {
      newl <- length(nodes[[l.nodes - i]])
      new.c.list <- vector(length = newl, mode = "list")
      new.s.list <- vector(length = newl, mode = "list")
      m <- c(0L, cumsum(nodes[[l.nodes - i]]))
      d0 <- 1/w[levels = l.nodes - i + 1L]
      for (j in 1:newl) {
        new.c.list[[j]] <- UpdateCw(c.list[(m[j] + 1L):m[j + 1L]], d1.vec, d0[j])
        y0 <- fcast[levels == l.nodes - i - 1L][j]
        w0 <- w[levels == l.nodes - i - 1L][j]
        new.s.list[[j]] <- y0 * w0 + unlist(sw.list[(m[j] + 1L):m[j + 1L]])
      }
      sw.list <- new.s.list
      c.list <- new.c.list
    }

    cmat <- c.list[[1L]]$cmat
    stwy <- sw.list[[1L]]
    dvec <- unlist(d.list)
    all.c[, , h] <- cmat
    tvec <- SumSplit(stwy * dvec, nodes[[l.nodes]])
    adj.fcast <- c(stwy - rep(cmat %*% tvec, nodes[[l.nodes]])) * dvec
    adj.fcasts[h, ] <- adj.fcast
  }
  return(adj.fcasts)  # Only return fcasts at the bottom level
}

# # Combination approach w/o weights
# # Author: Alan Lee

BasicC <- function(n)
{
  K <- length(n)
  C.list <- vector(length = K, mode = "list")
  for (i in 1L:K) 
    C.list[[i]] <- list(matrix(1L/(n[i] + 1L), 1L, 1L), n[i])
  return(C.list)
}

UpdateC <- function(C.list)
{
  # makes a new C matrix from those in the list C.list
  K <- length(C.list)
  # get dimensions of new matrix
  div <- 1L
  nvec <- numeric(K)
  comb.vec <- NULL
  for (i in 1L:K)
  {
    m <- C.list[[i]][[2L]]
    C <- C.list[[i]][[1L]]
    nvec[i] <- dim(C)[1L]
    div <- div - sum(m * (C %*% m))
    comb.vec <- c(comb.vec, m)
  }
  d <- sum(comb.vec)
  div <- div + d
  Cstar <- matrix(, sum(nvec), sum(nvec))
  # hi, lo for matrix insertion
  hi <- cumsum(nvec)
  lo <- cumsum(c(1L, nvec[-length(nvec)]))
  
  # update C matrix
  for (i in 1L:K)
  {
    Cm1 <- as.vector(C.list[[i]][[1L]] %*% C.list[[i]][[2L]])
    row.range <- lo[i]:hi[i]
    for (j in 1L:K)
    {
      Cm2 <- as.vector(C.list[[j]][[1L]] %*% C.list[[j]][[2L]])
      Cinsert <- outer(1L - Cm1, 1L - Cm2)/div
      if (i == j) 
        Cinsert <- C.list[[i]][[1L]] + Cinsert
      col.range <- lo[j]:hi[j]
      Cstar[row.range, col.range] <- Cinsert
    }
  }
  list(C = Cstar, nvec = comb.vec)
}


combinef <- function(fcasts, nList)
{
  # fcasts: Matrix of forecasts for all levels of the hierarchical time series.
  # Each row represents one forecast horizon and each column represents one time
  # series from the hierarchy, arranged in standard order ( descending the tree
  # from the root and moving across the levels from left to right)

  # g: the g-matrix describing the hierarchical tree structure

  # get level info
  nList <- c(1L, nList)
  L <- length(nList)
  
  # calculate level information for forecasts
  N <- sum(unlist(nList))
  Level <- rep(1L, N)
  
  node <- 2L
  for (l in 2L:L)
  {
    for (i in 1L:length(nList[[l]]))
    {
      for (j in 1L:nList[[l]][i])
      {
        Level[node] <- l
        node <- node + 1L
      }
    }
  }
  
  # initialise the lists at the bottom level
  
  # first inverse list
  C.list <- BasicC(nList[[L]])
  
  # then Sty lists, one for each forecast horizon
  newLength <- length(nList[[L]])
  
  # S list of lists
  S.list <- vector(length = nrow(fcasts), mode = "list")
  
  for (h in 1L:nrow(fcasts))
  {
    y <- fcasts[h, ]
    S.list[[h]] <- vector(length = newLength, mode = "list")
    m <- c(0L, cumsum(nList[[L]]))
    for (i in 1L:newLength)
    {
      yy <- y[Level == L][(m[i] + 1L):m[i + 1L]]
      S.list[[h]][[i]] <- yy + y[Level == L - 1L][i]
    }
  }
  
  # now iterate to construct the C and S list sequences
  new.S.list <- vector(length = nrow(fcasts), mode = "list")
  for (i in 1L:(L - 2L))
  {
    newLength <- length(nList[[L - i]])
    new.C.list <- vector(length = newLength, mode = "list")
    new.S.list <- vector(length = newLength, mode = "list")
    m <- c(0L, cumsum(nList[[L - i]]))
    for (h in 1L:nrow(fcasts))
    {
      y <- fcasts[h, ]
      for (j in 1L:newLength)
      {
        new.C.list[[j]] <- UpdateC(C.list[(m[j] + 1L):m[j + 1L]])
        y0 <- y[Level == L - i - 1L][j]
        new.S.list[[j]] <- y0 + unlist(S.list[[h]][(m[j] + 1L):m[j + 1L]])
      }
      S.list[[h]] <- new.S.list
    }
    C.list <- new.C.list
  }
  
  C <- C.list[[1L]]$C
  n <- nList[[L]]
  comb <- matrix(, nrow(fcasts), sum(n))
  for (h in 1L:nrow(fcasts))
  {
    STY <- unlist(S.list[[h]])
    # now solve normal equations
    sums <- tapply(STY, rep(1L:length(n), n), sum)
    comb[h, ] <- STY - rep(C %*% sums, n)
  }
  return(comb)
}

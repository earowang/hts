MiddleOut <- function(fcasts, nodes) {
  # Middle-out forecasts similar to tdfp
  levels <- c(0L, cumsum(sapply(nodes, sum)))
  # Split fcasts to a list
  l.levels <- length(levels) - 1L
  flist <- vector(length = l.levels, mode = "list")
  for (i in 1L:l.levels) {
    end <- levels[i + 1L]
    start <- levels[i] + 1L
    series <- seq(start, end)
    flist[[i]] <- fcasts[, series]
  }
  new.flist <- vector(length = l.levels - 1L, mode = "list")
  for (j in 1L:(l.levels - 1L)) {
    repcount <- rep(1:length(nodes[[j + 1L]]), nodes[[j + 1L]])
    new.flist[[j]] <- t(apply(flist[[j + 1L]], 1,
                              function(x) rowsum(x, repcount)))
  }
  tmp <- t(apply(new.flist[[1L]], 1, function(x) rep(x, nodes[[2L]])))
  prop <- flist[[2L]]/tmp
  mfcasts0 <- matrix(unlist(flist[[1L]]), ncol = ncol(flist[[1L]]))
  mfcasts <- t(apply(mfcasts0, 1, function(x) rep(x, nodes[[2L]])))
  if (l.levels > 2L) {
    for (k in 2L:(l.levels - 1L)) {
      prop <- t(apply(prop, 1, function(x) rep(x, nodes[[k + 1L]])))
      newprop <- t(apply(new.flist[[k]], 1, 
                         function(x) rep(x, nodes[[k + 1L]])))
      mfcasts <- t(apply(mfcasts, 1, function(x) rep(x, nodes[[k + 1L]])))
      prop <- prop * flist[[k + 1L]]/newprop
    }
  }
  out <- mfcasts * prop
  return(out)
}

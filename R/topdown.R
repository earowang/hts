# Top-down approaches only for hts
TdGsA <- function(fcasts, bts, topts) {
  # Top-down forecasts based on the average historical proportions. (Gross-Sohl
  # method A)
  div <- apply(bts, 2, function(x) x/topts)
  prop <- apply(div, 2, sum)/length(time(bts))
  out <- fcasts %*% prop
  return(out)
}

TdGsF <- function(fcasts, bts, topts) {
  # Top-down forecasts based on the proportions of the historical averages (
  # Gross-Sohl method F)
  numerator <- apply(bts, 2, sum)
  denominator <- sum(topts)
  prop <- numerator/denominator
  out <- fcasts %*% prop
  return(out)
}

TdFp <- function(allfcasts, nodes) {
  # Top-down forecasts using forecast proportions
  fcasts <- allfcasts[1L, ]
  levels <- cumsum(Mnodes(nodes))
  # Split fcasts to a list
  l.levels <- length(levels)
  flist <- vector(length = l.levels, mode = "list")
  flist[[1L]] <- fcasts[1L]
  for (i in 2L:l.levels) {
    end <- levels[i]
    start <- levels[i - 1L] + 1L
    series <- seq(start, end)
    flist[[i]] <- fcasts[series]
  }
  nodes <- c(1L, nodes)
  prop <- mapply("/", flist[[l.levels]], 
                 rep(flist[[l.levels - 1L]], nodes[[l.levels]]))
  for (j in (l.levels - 1L):2L) {
    tmp <- mapply("/", flist[[j]], rep(flist[[j - 1L]], nodes[[j]]))
    prop <- prop * rep(tmp)
  }
  out <- allfcasts[, 1L] * prop
  return(out)
}

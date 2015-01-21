# Top-down approaches only for hts
TdGsA <- function(fcasts, bts, topts) {
  # Top-down forecasts based on the average historical proportions. (Gross-Sohl
  # method A)
  div <- apply(bts, 2, function(x) x/topts)
  prop <- colMeans(div, na.rm = TRUE)
  out <- fcasts %*% prop
  return(out)
}

TdGsF <- function(fcasts, bts, topts) {
  # Top-down forecasts based on the proportions of the historical averages (
  # Gross-Sohl method F)
  numerator <- colSums(bts, na.rm = TRUE)
  denominator <- sum(topts, na.rm = TRUE)
  prop <- numerator/denominator
  out <- fcasts %*% prop
  return(out)
}

TdFp <- function(fcasts, nodes) {
  # Top-down forecasts using forecast proportions
  levels <- cumsum(Mnodes(nodes))
  # Split fcasts to a list
  l.levels <- length(levels)
  flist <- lapply(2L:l.levels, function(x) {
                    fcasts[, seq(levels[x - 1L] + 1L, levels[x])]
                  })
  flist <- c(list(fcasts[, 1L]), flist)
  if (is.vector(flist[[2L]])) {  # In case of h = 1
    new.flist <- vector(length = l.levels - 1L, mode = "list")
    for (j in 1L:(l.levels - 1L)) {
      repcount <- rep(1:length(nodes[[j]]), nodes[[j]])
      new.flist[[j]] <- rowsum(flist[[j + 1L]], repcount)
    }

    # Calculate proportions
    prop <- flist[[2L]]/new.flist[[1L]]
    if (l.levels > 2L) {
      for (k in 2L:(l.levels - 1L)) {
        prop <- rep(prop, nodes[[k]])
        newprop <- rep(new.flist[[k]], nodes[[k]])
        prop <- prop * flist[[k + 1L]]/newprop
      }
    }
    out <- t(fcasts[, 1L] * prop)
  } else {
    # Create the sum of the h-step-ahead base forecasts at l level above node j
    new.flist <- vector(length = l.levels - 1L, mode = "list")
    for (j in 1L:(l.levels - 1L)) {
      repcount <- rep(1:length(nodes[[j]]), nodes[[j]])
      new.flist[[j]] <- t(apply(flist[[j + 1L]], 1, 
                                function(x) rowsum(x, repcount)))
    }

    # Calculate proportions
    prop <- apply(flist[[2L]], 2, function(x) x/new.flist[[1L]])
    if (l.levels > 2L) {
      for (k in 2L:(l.levels - 1L)) {
        prop <- t(apply(prop, 1, function(x) rep(x, nodes[[k]])))
        newprop <- t(apply(new.flist[[k]], 1, function(x) rep(x, nodes[[k]])))
        prop <- prop * flist[[k + 1L]]/newprop
      }
    }
    out <- fcasts[, 1L] * prop
  }
  return(out)
}

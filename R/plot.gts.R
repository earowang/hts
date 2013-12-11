plot.gts <- function(xts, include, levels, labels = TRUE, ...) {
  # Do plotting
  #
  # Args:
  #   xts: hts or gts
  #   include: No. of historical data included in the plot.
  #   levels: which level or group to display.
  #   labels: text labels
  #
  # Return:
  #   hts or gts plots
  #
  # Error Handling:
  if (!is.gts(xts)) {
    stop("Argument xts must be either hts or gts object.")
  }

  if (!is.null(xts$histy)) {
    histx <- aggts(xts, levels, forecast = FALSE)
    fcasts <- aggts(xts, levels, forecast = TRUE)
  } else {
    histx <- aggts(xts, levels)
  }

  if (missing(include)) {
    histx <- histx
    include <- end(histx)[1L] - start(histx)[1L] + 1L
  } else {
    histx <- window(histx, start = end(histx)[1L] - include + 1L)
  }

  if (missing(levels)) {
    if (is.hts(xts)) {
      levels <- 0L:length(xts$nodes)
    } else {
      levels <- 0L:(nrow(xts$groups) - 1L)
    }
  }

  l.levels <- length(levels)
  levels <- as.integer(levels) + 1L

  opar <- par(mfrow = c(l.levels, 1L), mar = c(3, 4, 4, 2))
  on.exit(par(opar))

  if (is.hts(xts)) {
    m <- Mnodes(xts$nodes)[levels]
  } else {
    m <- apply(xts$groups, 1, function(x) length(unique(x)))[levels]
    xts$labels <- c(Total = "Total", xts$labels, 
                    Bottom = list(colnames(xts$bts)))
  }

  cs <- c(0L, cumsum(m))
  
  for (i in 1L:l.levels) { 
    end <- cs[i + 1L]
    start <- cs[i] + 1L
    series <- seq(start, end)
    cols <- rainbow(length(series))
    if(!is.null(xts$histy)) {
      ylim <- range(histx[, series], fcasts[, series])
      xlim <- range(time(histx), time(fcasts))
    } else {
      ylim <- range(histx[, series])
      xlim <- range(time(histx))
    }
    plot(histx[, series, drop = FALSE], col = cols, xlim = xlim, ylim = ylim, 
         xlab = "", ylab = "", main = names(xts$labels)[levels][i], 
         plot.type = "single", type = ifelse(include == 1L, "p", "l"), ...)

    if (!is.null(xts$histy)) {
      for (j in 1L:length(series)) {
        lines(fcasts[, series[j], drop = FALSE], lty = 2, col = cols[j], 
              type = ifelse(nrow(fcasts) == 1L, "p", "l"))
      }
    }

    if (labels) {
      text(x = tsp(histx)[1] + 0.1, y = histx[1, series] + 0.2,
           labels = unlist(xts$labels[levels][i]), 
           cex = 0.9, adj = 1)
    }
  }
}

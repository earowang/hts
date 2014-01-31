plot.gts <- function(x, include, levels, labels = TRUE, ...) {
  # Do plotting
  #
  # Args:
  #   x: hts or gts
  #   include: No. of historical data included in the plot.
  #   levels: which level or group to display.
  #   labels: text labels
  #
  # Return:
  #   hts or gts plots
  #
  # Error Handling:
  if (!is.gts(x)) {
    stop("Argument x must be either hts or gts object.")
  }

  if (!is.null(x$histy)) {
    histx <- aggts(x, levels, forecasts = FALSE)
    fcasts <- aggts(x, levels, forecasts = TRUE)
  } else {
    histx <- aggts(x, levels)
  }

  if (missing(include)) {
    histx <- histx
    include <- end(histx)[1L] - start(histx)[1L] + 1L
  } else {
    histx <- window(histx, start = end(histx)[1L] - include + 1L)
  }

  if (missing(levels)) {
    if (is.hts(x)) {
      levels <- 0L:length(x$nodes)
    } else {
      levels <- 0L:(nrow(x$groups) - 1L)
    }
  }

  l.levels <- length(levels)
  if (is.character(levels)) {
    levels <- which(names(x$labels) %in% levels)
  }
  levels <- as.integer(levels) + 1L

  opar <- par(mfrow = c(l.levels, 1L), mar = c(3, 4, 4, 2))
  on.exit(par(opar))

  if (is.hts(x)) {
    m <- Mnodes(x$nodes)[levels]
  } else {
    m <- Mlevel(x$groups)[levels]
    x$labels <- c(Total = "Total", x$labels, 
                    Bottom = list(colnames(x$bts)))
  }

  cs <- c(0L, cumsum(m))
  
  for (i in 1L:l.levels) { 
    end <- cs[i + 1L]
    start <- cs[i] + 1L
    series <- seq(start, end)
    cols <- rainbow(length(series))
    if(!is.null(x$histy)) {
      ylim <- range(histx[, series], fcasts[, series], na.rm = TRUE)
      xlim <- range(time(histx), time(fcasts), na.rm = TRUE)
    } else {
      ylim <- range(histx[, series], na.rm = TRUE)
      xlim <- range(time(histx), na.rm = TRUE)
    }
    plot(histx[, series, drop = FALSE], col = cols, xlim = xlim, ylim = ylim, 
         xlab = "", ylab = "", main = names(x$labels)[levels][i], 
         plot.type = "single", type = ifelse(include == 1L, "p", "l"), ...)

    if (!is.null(x$histy)) {
      for (j in 1L:length(series)) {
        lines(fcasts[, series[j], drop = FALSE], lty = 2, col = cols[j], 
              type = ifelse(nrow(fcasts) == 1L, "p", "l"))
      }
    }

    if (labels) {
      text(x = tsp(histx)[1] + 0.1, y = histx[1, series] + 0.2,
           labels = unlist(x$labels[levels][i]), 
           cex = 0.9, adj = 1)
    }
  }
}

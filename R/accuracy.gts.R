accuracy.gts <- function(fcasts, test, levels) {
  # Compute in-sample or out-of-sample accuracy measures
  #
  # Args:
  #   fcasts: forcasts
  #   test: Test set. If it's missing, default is in-sample accuracy for the
  #         bottom level, when keep.fitted is set to TRUE in the forecast.gts().
  #   levels: If computing out-of-sample accuracy, users can select whatever
  #           levels they like.
  #
  # Returns:
  #   Accuracy measures
  #
  # Error Handling:
  if (!is.gts(fcasts)) {
    stop("Argument fcasts must be a grouped time series.")
  }
  if (!missing(test) && !is.gts(test)) {
    stop("Argument test must be a grouped time series.")
  }

  if (missing(test))
  {
    if(is.null(fcasts$fitted))
      stop("No fitted values available for historical times, and no actual values available for future times")

    x <- unclass(fcasts$histy)  # Unclass mts to matrix
    res <- x - unclass(fcasts$fitted)  # fcasts$residuals may contain errors
    levels <- ifelse(is.hts(fcasts), length(fcasts$nodes),
                     nrow(fcasts$groups) - 1L)
  }
  else {
    f <- unclass(aggts(fcasts, levels, forecasts = TRUE))
    x <- unclass(aggts(test, levels))
    res <- x - f
  }

  if(is.null(fcasts$histy))
    histy <- NULL
  else
    histy <- aggts(fcasts, levels, forecasts = FALSE)
  if (!is.null(histy)) {
    scale <- colMeans(abs(diff(histy, lag = max(1, stats::frequency(histy)))),
                      na.rm = TRUE)
    q <- sweep(res, 2, scale, "/")
    mase <- colMeans(abs(q), na.rm = TRUE)
  }
  pe <- res/x * 100  # percentage error

  me <- colMeans(res, na.rm = TRUE)
  rmse <- sqrt(colMeans(res^2, na.rm = TRUE))
  mae <- colMeans(abs(res), na.rm = TRUE)
  mape <- colMeans(abs(pe), na.rm = TRUE)
  mpe <- colMeans(pe, na.rm = TRUE)

  out <- rbind(me, rmse, mae, mape, mpe)
  rownames(out) <- c("ME", "RMSE", "MAE", "MAPE", "MPE")
  if (exists("mase")) {
    out <- rbind(out, mase)
    rownames(out)[6L] <- "MASE"
  }
  if (exists("f")) {
    colnames(out) <- colnames(f)
  }
  return(out)
}

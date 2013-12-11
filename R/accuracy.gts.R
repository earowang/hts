accuracy.gts <- function(fcasts, test, levels) {
  # Compute in-sample or out-of-sample accuracy measures 
  #
  # Args:
  #   fcasts: forcasts
  #   test: Test set. If it's missing, default is in-sample accuracy for the
  #         bottom level, when keep.resid is set to TRUE in the forecast.gts().
  #   levels: If computing out-of-sample accuracy, users can select whatever
  #           levels they like.
  #
  # Returns:
  #   Accuracy measures
  #
  # Error Handling:
  if (!is.gts(fcasts)) {
    stop("Arguments fcasts and test should be a grouped time series.")
  }
  if (is.null(fcasts$histy) && !missing(test)) {
    stop("Unable to compute forecast accuracy measures.")
  }

  if (missing(test) && exists("residuals")) {
    x <- fcasts$histy
    res <- fcasts$residuals
  } else {
    f <- aggts(fcasts, levels)
    x <- aggts(test, levels)
    res <- x - f
  }

  pe <- res/x * 100  # percentage error
  scale <- colMeans(diff(x, lag = max(1, frequency(x))), na.rm = TRUE)
  q <- sweep(res, 2, scale, "/")

  me <- colMeans(res, na.rm = TRUE)
  rmse <- sqrt(colMeans(res^2, na.rm = TRUE))
  mae <- colMeans(abs(res), na.rm = TRUE)
  mape <- colMeans(abs(pe), na.rm = TRUE)
  mpe <- colMeans(pe, na.rm = TRUE)
  mase <- colMeans(abs(q), na.rm = TRUE)

  out <- rbind(me, rmse, mae, mape, mpe, mase)
  if (exists("f")) {
    colnames(out) <- colnames(f)
  }
  rownames(out) <- c("ME", "RMSE", "MAE", "MAPE", "MPE", "MASE")
  return(out)
}

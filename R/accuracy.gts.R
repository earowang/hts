accuracy.gts <- function(fcasts, test, levels) {
  # Compute forecast accuracy measures 
  if (!is.gts(fcasts) && !is.gts(test)) {
    stop("Arguments fcasts and test should be a grouped time series.")
  }
  if (is.null(fcasts$histy)) {
    stop("Argument fcasts should be a forecast grouped time series.")
  }

  f <- aggts(fcasts, levels)
  x <- aggts(test, levels)
  res <- x - f
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
  colnames(out) <- colnames(f)
  rownames(out) <- c("ME", "RMSE", "MAE", "MAPE", "MPE", "MASE")
  return(out)
}

#' In-sample or out-of-sample accuracy measures for forecast grouped and
#' hierarchical model
#' 
#' Returns a range of summary measures of the forecast accuracy. The function
#' measures out-of-sample forecast accuracy based on (holdout data - forecasts)
#' and in-sample accuracy at the bottom level when setting \code{keep.fitted =
#' TRUE} in the \code{\link[hts]{forecast.gts}}. All measures are defined and
#' discussed in Hyndman and Koehler (2006).
#' 
#' MASE calculation is scaled using MAE of in-sample naive forecasts for
#' non-seasonal time series, and in-sample seasonal naive forecasts for
#' seasonal time series.
#' 
#' @param f An object of class \code{gts}, containing the forecasted
#' hierarchical or grouped time series. In-sample accuracy at the bottom level
#' returns when \code{test} is missing.
#' @param test An object of class \code{gts}, containing the holdout
#' hierarchical time series
#' @param levels Return the specified level(s), when carrying out out-of-sample
#' @param ...  Extra arguments to be ignored
#' @return Matrix giving forecast accuracy measures. \item{ME}{Mean Error}
#' \item{RMSE}{Root Mean Square Error} \item{MAE}{Mean Absolute Error}
#' \item{MAPE}{Mean Absolute Percentage Error} \item{MPE}{Mean Percentage
#' Error} \item{MASE}{Mean Absolute Scaled Error}
#' @author Rob J Hyndman and Earo Wang
#' @seealso \code{\link[hts]{hts}}, \code{\link[hts]{plot.gts}},
#' \code{\link[hts]{forecast.gts}}, \code{\link[forecast]{accuracy}}
#' @references R. J. Hyndman and A. Koehler (2006), Another look at measures of
#' forecast accuracy, \emph{International Journal of Forecasting}, \bold{22},
#' 679-688.
#' @keywords error
#' @method accuracy gts
#' @examples
#' 
#' data <- window(htseg2, start = 1992, end = 2002)
#' test <- window(htseg2, start = 2003)
#' fcasts <- forecast(data, h = 5, method = "bu")
#' accuracy(fcasts, test)
#' accuracy(fcasts, test, levels = 1)
#' 
#' @export
#' @export accuracy.gts
accuracy.gts <- function(f, test, levels, ...) {
  # Compute in-sample or out-of-sample accuracy measures
  #
  # Args:
  #   f: forcasts
  #   test: Test set. If it's missing, default is in-sample accuracy for the
  #         bottom level, when keep.fitted is set to TRUE in the forecast.gts().
  #   levels: If computing out-of-sample accuracy, users can select whatever
  #           levels they like.
  #
  # Returns:
  #   Accuracy measures
  #
  # Error Handling:
  if (!is.gts(f)) {
    stop("Argument f must be a grouped time series.", call. = FALSE)
  }
  if (!missing(test) && !is.gts(test)) {
    stop("Argument test must be a grouped time series.", call. = FALSE)
  }

  if (missing(test))
  {
    if(is.null(f$fitted))
      stop("No fitted values available for historical times, and no actual values available for future times", call. = FALSE)

    x <- unclass(f$histy)  # Unclass mts to matrix
    res <- x - unclass(f$fitted)  # f$residuals may contain errors
    levels <- ifelse(is.hts(f), length(f$nodes),
                     nrow(f$groups) - 1L)
  }
  else {
    fcasts <- unclass(aggts(f, levels, forecasts = TRUE))
    x <- unclass(aggts(test, levels))
    res <- x - fcasts
  }

  if(is.null(f$histy))
    histy <- NULL
  else
    histy <- aggts(f, levels, forecasts = FALSE)
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
  if (exists("fcasts")) {
    colnames(out) <- colnames(fcasts)
  }
  return(out)
}

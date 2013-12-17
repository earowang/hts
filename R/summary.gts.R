summary.gts <- function(xts, ...) {
  print(xts)
  if (is.null(xts$histy)) {
    cat("\n")
    cat("Labels: \n")
    print(names(xts$labels))
  } else {
    method <- switch(xts$method,
    comb = "Optimal combination forecasts",
    bu = "Bottom-up forecasts",
    mo = "Middle-out forecasts",
    tdgsa = "Top-down forecasts based on the average historical proportions",
    tdgsf = "Top-down forecasts based on the proportion of historical averages",
    tdfp = "Top-down forecasts using forecasts proportions")
    fmethod <- switch(xts$fmethod, ets = "ETS", arima = "Arima", 
                      rw = "Random walk")
    cat("\n")
    cat(paste("Method:", method), "\n")
    cat(paste("Forecast method:", fmethod), "\n")
    if (!is.null(xts$fitted)) {
      cat("In-sample error measures at the bottom level: \n")
      print(accuracy.gts(xts))
    }
  }
}

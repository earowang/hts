summary.gts <- function(object, ...) {
  print(object)
  if (is.null(object$histy)) {
    cat("\n")
    cat("Labels: \n")
    print(names(object$labels))
  } else {
    method <- switch(object$method,
    comb = "Optimal combination forecasts",
    bu = "Bottom-up forecasts",
    mo = "Middle-out forecasts",
    tdgsa = "Top-down forecasts based on the average historical proportions",
    tdgsf = "Top-down forecasts based on the proportion of historical averages",
    tdfp = "Top-down forecasts using forecasts proportions")
    fmethod <- switch(object$fmethod, ets = "ETS", arima = "Arima", 
                      rw = "Random walk")
    cat("\n")
    cat(paste("Method:", method), "\n")
    cat(paste("Forecast method:", fmethod), "\n")
    if (!is.null(object$fitted)) {
      cat("In-sample error measures at the bottom level: \n")
      print(accuracy.gts(object))
    }
  }
}

summary.gts <- function(fcasts, ...) {
  # Summary function only used for forecasts
  method <- switch(fcasts$method,
                   comb = "Optimal combination forecasts",
                   bu = "Bottom-up")
  fmethod <- switch(fcasts$fmethod,
                    ets = "ETS",
                    arima = "Arima",
                    rw = "Random walk")
  print(fcasts)
  cat("\n")
  cat(paste("Method:", method), "\n")
  cat(paste("Forecast method:", fmethod), "\n")
  if (!is.null(fcasts$fitted)) {
    cat("Error measures at the bottom level: \n")
    print(accuracy.gts(fcasts))
  }
}

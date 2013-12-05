forecast.gts <- function(object, h = ifelse(frequency(object) > 1L, 
                         2L*frequency(object), 10L), method = c("bu"),
                         fmethod = c("ets"), ...) {
  # Forecast hts or gts objects
  #
  # Args:
  #   object*: Only hts/gts can be passed onto this function.
  #   h: h-step forecasts.
  #   method: Aggregated approaches.
  #   fmethod: Forecast methods.
  #
  # Return:
  #   Point forecasts with other info chosen by the user.
  #
  require(forecast)
  method <- match.arg(method)
  fmethod <- match.arg(fmethod)
  # Error Handling:
  if (!is.gts(object)) {
    stop("Argument object must be either a hts or gts object.")
  }
  if (h < 1L) {
    stop("Argument h must be positive.")
  }

  # Bottom-up approach
  if (method == "bu") {
    ally <- object$bts  # Only grab the bts
  }
  
  if (fmethod == "ets") {
    # Fit a model
    fit <- sapply(ally, function(x) ets(x, ...), simplify = FALSE)
    # Generate point forecasts at the bottom level
    fcasts <- lapply(fit, function(x) forecast(x, h = h)$mean)
  }

  if (method == "bu") {
    
  }
}

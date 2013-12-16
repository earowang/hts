# Top-down approaches
TdGsA <- function(fcasts, bts, topts) {
  # Top-down forecasts based on the average historical proportions. (Gross-Sohl
  # method A)
  div <- sweep(bts, 2, topts, "/", check.margin = FALSE)
  prop <- apply(div, 2, sum)/length(time(bts))
  out <- fcasts %*% prop
  return(out)
}

# Top-down approaches only for hts
TdGsA <- function(fcasts, bts, topts) {
  # Top-down forecasts based on the average historical proportions. (Gross-Sohl
  # method A)
  div <- sweep(bts, 2, topts, "/", check.margin = FALSE)
  prop <- apply(div, 2, sum)/length(time(bts))
  out <- fcasts %*% prop
  return(out)
}

TdGsF <- function(fcasts, bts, topts) {
  # Top-down forecasts based on the proportions of the historical averages (
  # Gross-Sohl method F)
  numerator <- apply(bts, 2, sum)
  denominator <- sum(topts)
  prop <- numerator/denominator
  out <- fcasts %*% prop
  return(out)
}

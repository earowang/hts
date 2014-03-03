# gts example
plot(infantgts, levels = 1)

fcasts3.comb <- forecast(infantgts, h = 4, method = "comb", fmethod = "ets")
aggts(fcasts3.comb, levels = 1)
aggts(fcasts3.comb, levels = 1, forecasts = FALSE)
plot(fcasts3.comb)
plot(fcasts3.comb, include = 5, levels = c(1, 2))

fcasts3.combsd <- forecast(infantgts, h = 4, method = "comb", fmethod = "ets",
                           weights = "sd", keep.fitted = TRUE)
aggts(fcasts3.combsd, levels = 1)
summary(fcasts3.combsd)

fcasts3.combn <- forecast(infantgts, h = 4, method = "comb", fmethod = "ets",
                          weights = "nseries", keep.resid = TRUE)
aggts(fcasts3.combn, levels = 3)

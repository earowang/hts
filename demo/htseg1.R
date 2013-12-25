# hts example 1
print(htseg1)
summary(htseg1)
aggts(htseg1)
aggts(htseg1, levels = 1)
aggts(htseg1, levels = c(0, 2))
plot(htseg1, levels = 1)
smatrix(htseg1)  # Return the dense mode

# Forecasts
fcasts1.bu.p <- forecast(htseg1, h = 4, method = "bu", fmethod = "ets", 
                       parallel = TRUE)
aggts(fcasts1.bu.p)
fcasts1.bu <- forecast(htseg1, h = 4, method = "bu", fmethod = "ets", 
                       parallel = FALSE)
aggts(fcasts1.bu)
summary(fcasts1.bu)
attributes(fcasts1.bu)
fcasts1.td <- forecast(htseg1, h = 4, method = "tdfp", fmethod = "arima",
                       keep.fitted = TRUE)
attributes(fcasts1.td)  # "fitted" there
summary(fcasts1.td)  # When keep.fitted = TRUE, return in-sample accuracy
fcasts1.comb <- forecast(htseg1, h = 4, method = "comb", fmethod = "ets",
                         keep.fitted = TRUE)
aggts(fcasts1.comb)
summary(fcasts1.comb)  
print(fcasts1.comb)  
plot(fcasts1.comb, levels = 2)
plot(fcasts1.comb, include = 5, levels = c(1, 2))

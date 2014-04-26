# hts example 2
data <- window(htseg2, start = 1992, end = 2002)
test <- window(htseg2, start = 2003)
fcasts2.mo <- forecast(data, h = 5, method = "mo", fmethod = "ets", level = 1,
                       keep.fitted = TRUE, keep.resid = TRUE)
accuracy.gts(fcasts2.mo, test)
accuracy.gts(fcasts2.mo, test, levels = 1)
fcasts2.td <- forecast(data, h = 5, method = "tdgsa", fmethod = "ets", 
                       keep.fitted = TRUE, keep.resid = TRUE)
summary(fcasts2.td)
plot(fcasts2.td, include = 5)
plot(fcasts2.td, include = 5, levels = c(0, 2))


<!-- README.md is generated from README.Rmd. Please edit that file -->
hts
===

[![Travis-CI Build Status](https://travis-ci.org/earowang/hts.svg?branch=master)](https://travis-ci.org/earowang/hts) [![codecov](https://codecov.io/gh/earowang/hts/branch/master/graph/badge.svg)](https://codecov.io/gh/earowang/hts) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/hts)](https://cran.r-project.org/package=hts) [![Downloads](http://cranlogs.r-pkg.org/badges/hts)](https://cran.r-project.org/package=hts) [![Pending Pull-Requests](http://githubbadges.herokuapp.com/earowang/hts/pulls.svg?style=flat)](https://github.com/earowang/hts/pulls)

The R package *hts* presents functions to create, plot and forecast hierarchical and grouped time series.

Installation
------------

You can install the **stable** version on [R CRAN](https://cran.r-project.org/package=hts).

``` r
install.packages('hts', dependencies = TRUE)
```

You can also install the **development** version from [Github](https://github.com/robjhyndman/gts)

``` r
# install.packages("devtools")
devtools::install_github("earowang/hts")
```

Usage
-----

### Example 1: hierarchical time series

``` r
library(hts)
#> Loading required package: forecast

# hts example 1
print(htseg1)
#> Hierarchical Time Series 
#> 3 Levels 
#> Number of nodes at each level: 1 2 5 
#> Total number of series: 8 
#> Number of observations per series: 10 
#> Top level series: 
#> Time Series:
#> Start = 1992 
#> End = 2001 
#> Frequency = 1 
#>  [1] 48.74808 49.48047 49.93238 50.24070 50.60846 50.84851 51.70922
#>  [8] 51.94330 52.57796 53.21496
summary(htseg1)
#> Hierarchical Time Series 
#> 3 Levels 
#> Number of nodes at each level: 1 2 5 
#> Total number of series: 8 
#> Number of observations per series: 10 
#> Top level series: 
#> Time Series:
#> Start = 1992 
#> End = 2001 
#> Frequency = 1 
#>  [1] 48.74808 49.48047 49.93238 50.24070 50.60846 50.84851 51.70922
#>  [8] 51.94330 52.57796 53.21496
#> 
#> Labels: 
#> [1] "Level 0" "Level 1" "Level 2"
aggts1 <- aggts(htseg1)
aggts2 <- aggts(htseg1, levels = 1)
aggts3 <- aggts(htseg1, levels = c(0, 2))
plot(htseg1, levels = 1)
```

![](figure/hts-eg1-1.png)

``` r
smatrix(htseg1)  # Return the dense mode
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    1    1    1    1
#> [2,]    1    1    1    0    0
#> [3,]    0    0    0    1    1
#> [4,]    1    0    0    0    0
#> [5,]    0    1    0    0    0
#> [6,]    0    0    1    0    0
#> [7,]    0    0    0    1    0
#> [8,]    0    0    0    0    1

# Forecasts
fcasts1.bu <- forecast(
  htseg1, h = 4, method = "bu", fmethod = "ets", parallel = TRUE
)
aggts4 <- aggts(fcasts1.bu)
summary(fcasts1.bu)
#> Hierarchical Time Series 
#> 3 Levels 
#> Number of nodes at each level: 1 2 5 
#> Total number of series: 8 
#> Number of observations in each historical series: 10 
#> Number of forecasts per series: 4 
#> Top level series of forecasts: 
#> Time Series:
#> Start = 2002 
#> End = 2005 
#> Frequency = 1 
#> [1] 53.2149 53.2149 53.2149 53.2149
#> 
#> Method: Bottom-up forecasts 
#> Forecast method: ETS
fcasts1.td <- forecast(
  htseg1, h = 4, method = "tdfp", fmethod = "arima", keep.fitted = TRUE
)
summary(fcasts1.td)  # When keep.fitted = TRUE, return in-sample accuracy
#> Hierarchical Time Series 
#> 3 Levels 
#> Number of nodes at each level: 1 2 5 
#> Total number of series: 8 
#> Number of observations in each historical series: 10 
#> Number of forecasts per series: 4 
#> Top level series of forecasts: 
#> Time Series:
#> Start = 2002 
#> End = 2005 
#> Frequency = 1 
#> [1] 53.71128 54.20760 54.70392 55.20024
#> 
#> Method: Top-down forecasts using forecasts proportions 
#> Forecast method: Arima 
#> In-sample error measures at the bottom level: 
#>                AA           AB          AC          BA           BB
#> ME   0.0007719336 0.0009183738 0.001003812 0.001043247  0.001087807
#> RMSE 0.1298400018 0.0515879830 0.040306867 0.037462277  0.105015065
#> MAE  0.0978321731 0.0436089571 0.033210387 0.027003846  0.081906948
#> MAPE 1.1275970221 0.4534439625 0.323535559 0.251066115  0.691364891
#> MPE  0.0367879336 0.0069220593 0.006785872 0.007787895 -0.011087494
#> MASE 0.6825678136 0.5197483057 0.774250880 0.447950006  0.493684443
fcasts1.comb <- forecast(
  htseg1, h = 4, method = "comb", fmethod = "ets", keep.fitted = TRUE
)
aggts4 <- aggts(fcasts1.comb)
print(fcasts1.comb)  
#> Hierarchical Time Series 
#> 3 Levels 
#> Number of nodes at each level: 1 2 5 
#> Total number of series: 8 
#> Number of observations in each historical series: 10 
#> Number of forecasts per series: 4 
#> Top level series of forecasts: 
#> Time Series:
#> Start = 2002 
#> End = 2005 
#> Frequency = 1 
#> [1] 53.52186 53.95209 54.38231 54.81253
plot(fcasts1.comb, levels = 2)
```

![](figure/hts-eg1-2.png)

``` r
plot(fcasts1.comb, include = 5, levels = c(1, 2))
```

![](figure/hts-eg1-3.png)

### Example 2: hierarchical time series

``` r
# hts example 2
data <- window(htseg2, start = 1992, end = 2002)
test <- window(htseg2, start = 2003)
fcasts2.mo <- forecast(
  data, h = 5, method = "mo", fmethod = "ets", level = 1,
  keep.fitted = TRUE, keep.resid = TRUE
)
accuracy.gts(fcasts2.mo, test)
#>            Total          A          B        A10         A20         B30
#> ME    -0.1794783 -0.2486428 0.06916451 -0.1954860 -0.05315684 -0.02399186
#> RMSE   0.1818643  0.2732218 0.13585344  0.2180641  0.05572214  0.03144802
#> MAE    0.1794783  0.2486428 0.11251209  0.1954860  0.05315684  0.02399186
#> MAPE  11.1268019  8.4004312 2.27814495  7.5371465 14.49009308  1.69936058
#> MPE  -11.1268019  8.4004312 1.30221674  7.5371465 14.49009308 -1.69936058
#> MASE   0.5663497  1.3950200 0.81137939  1.2966219  1.93505737  0.52639468
#>             B40        A10A       A10B         A10C        A20A
#> ME   0.09315637 -0.03774880 -0.1649411  0.007203920 -0.04105604
#> RMSE 0.16498366  0.05205754  0.1779641  0.012753680  0.04696434
#> MAE  0.13636607  0.03873088  0.1649411  0.009838328  0.04105604
#> MAPE 3.85101876  3.11436103 20.5656022  1.774154141 13.39804861
#> MPE  2.45900161  3.04078646 20.5656022 -1.351777719 13.39804861
#> MASE 1.46488456  0.36661690  5.1038016  0.768361455  3.23432001
#>             A20B         B30A       B30B       B30C       B40A      B40B
#> ME   -0.01210080  -0.05065830 0.01324339 0.01342306 -0.0495926 0.1427490
#> RMSE  0.01432307   0.05944454 0.01485743 0.01599316  0.0728362 0.2349335
#> MAE   0.01228684   0.05065830 0.01324339 0.01342306  0.0495926 0.1800119
#> MAPE 15.88898042  16.89941739 2.89472660 2.07537753  3.6008092 8.1543683
#> MPE  15.22048749 -16.89941739 2.89472660 2.07537753 -3.6008092 6.1053561
#> MASE  0.83150955   2.07454397 1.47284424 1.10323688  0.9554614 4.3707453
accuracy.gts(fcasts2.mo, test, levels = 1)
#>               A          B
#> ME   -0.2486428 0.06916451
#> RMSE  0.2732218 0.13585344
#> MAE   0.2486428 0.11251209
#> MAPE  8.4004312 2.27814495
#> MPE   8.4004312 1.30221674
#> MASE  1.3950200 0.81137939
fcasts2.td <- forecast(
  data, h = 5, method = "tdgsa", fmethod = "ets", 
  keep.fitted = TRUE, keep.resid = TRUE
)
summary(fcasts2.td)
#> Hierarchical Time Series 
#> 4 Levels 
#> Number of nodes at each level: 1 2 4 10 
#> Total number of series: 17 
#> Number of observations in each historical series: 11 
#> Number of forecasts per series: 5 
#> Top level series of forecasts: 
#> Time Series:
#> Start = 2003 
#> End = 2007 
#> Frequency = 1 
#> [1] 1.273401 1.588504 1.903607 2.218710 2.533814
#> 
#> Method: Top-down forecasts based on the average historical proportions 
#> Forecast method: ETS 
#> In-sample error measures at the bottom level: 
#>            A10A       A10B       A10C        A20A        A20B         B30A
#> ME    -3.078268  -1.745391  -1.224382  -0.7686908  -0.3310768    0.3292895
#> RMSE   4.965263   2.799012   1.958883   1.2352996   0.5443681    0.5296102
#> MAE    3.748304   2.125358   1.482615   0.9377466   0.4161939    0.3985344
#> MAPE 188.887337 200.346295 207.045750 200.7373009 199.8010762 1541.6396082
#> MPE  140.564159 155.447413 166.179284 155.4362954 127.9841464 1513.8144177
#> MASE  35.480518  65.765326 115.790440  73.8739792  28.1658362   16.3206628
#>             B30B       B30C       B40A       B40B
#> ME     0.7266444   1.042012   1.840709   3.212842
#> RMSE   1.1756978   1.684995   2.982228   5.198359
#> MAE    0.8894648   1.278269   2.243009   3.945942
#> MAPE 254.0019540 252.473588 282.009560 253.691729
#> MPE  214.7989198 212.414710 247.233647 212.910572
#> MASE  98.9205307 105.060497  43.214274  95.808712
plot(fcasts2.td, include = 5)
```

![](figure/hts-eg2-1.png)

``` r
plot(fcasts2.td, include = 5, levels = c(0, 2))
```

![](figure/hts-eg2-2.png)

### Example 3: grouped time series

``` r
# gts example
plot(infantgts, levels = 1)
```

![](figure/gts-eg-1.png)

``` r

fcasts3.comb <- forecast(infantgts, h = 4, method = "comb", fmethod = "ets")
agg_gts1 <- aggts(fcasts3.comb, levels = 1)
agg_gts2 <- aggts(fcasts3.comb, levels = 1, forecasts = FALSE)
plot(fcasts3.comb)
```

![](figure/gts-eg-2.png)

``` r
plot(fcasts3.comb, include = 5, levels = c(1, 2))
```

![](figure/gts-eg-3.png)

``` r

fcasts3.combsd <- forecast(
  infantgts, h = 4, method = "comb", fmethod = "ets",
  weights = "sd", keep.fitted = TRUE
)
summary(fcasts3.combsd)
#> Grouped Time Series 
#> 4 Levels 
#> Number of groups at each level: 1 2 8 16 
#> Total number of series: 27 
#> Number of observations in each historical series: 71 
#> Number of forecasts per series: 4 
#> Top level series of forecasts: 
#> Time Series:
#> Start = 2004 
#> End = 2007 
#> Frequency = 1 
#> [1] 1186.251 1157.514 1128.776 1100.038
#> 
#> Method: Optimal combination forecasts 
#> Forecast method: ETS 
#> In-sample error measures at the bottom level: 
#>       NSW female VIC female QLD female  SA female  WA female   NT female
#> ME    2.67801971  -3.891006  3.6842227 -0.9442843 -0.3160882   0.2229132
#> RMSE 49.63474255  39.926658 29.3232649 17.8752210 19.1821988  11.8457268
#> MAE  36.45890700  30.690563 22.5996456 12.6073679 14.6690266   7.5225635
#> MAPE  6.65729877   8.723297  9.2855552 11.8658597 12.3364268  59.4921825
#> MPE   0.01485184  -2.199968  0.2070257 -3.3637430 -2.2944685 -27.8184522
#> MASE  0.90855233   0.939781  0.7909876  0.8059505  0.9023127   1.1275791
#>      ACT female TAS female   NSW male   VIC male  QLD male    SA male
#> ME    0.6957455 -1.9691405 -8.8518517 -6.5942736 -2.075320 -1.9874845
#> RMSE  4.4518673 12.0671580 63.4746798 47.4415369 27.130488 21.1063089
#> MAE   3.4097890  8.6528343 47.4780637 37.6812780 21.251206 15.6553245
#> MAPE        Inf 14.8631941  7.0916256  8.3730732  7.027799 12.3787569
#> MPE        -Inf -6.1664536 -2.3270206 -2.3400289 -1.410862 -4.4955258
#> MASE  0.8345637  0.8240795  0.9203723  0.9430424  0.947506  0.8601827
#>         WA male    NT male    ACT male   TAS male
#> ME   -1.7575813   0.484230  -0.3857071 -2.3084463
#> RMSE 19.0321452  13.425137   5.5750925 11.9823403
#> MAE  14.4176042   8.240797   4.0969108  8.5820839
#> MAPE  9.7699651  49.996253  49.9145862 14.8631814
#> MPE  -2.9231241 -14.695598 -30.8042542 -6.8844239
#> MASE  0.9027122   1.243224   0.8240913  0.8173413

fcasts3.combn <- forecast(
  infantgts, h = 4, method = "comb", fmethod = "ets",
  weights = "nseries", keep.resid = TRUE
)
```

License
-------

This package is free and open source software, licensed under GPL (&gt;= 2).

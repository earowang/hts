# hts

The R package *hts* presents functions to create, plot and forecast hierarchical 
and grouped time series. In forecasting hierarchical and grouped time series, the 
base methods implemented include ETS, ARIMA and the naive (random walk) models. 
Forecasts for grouped time series are calibrated using bottom-up and optimal 
combination methods. Forecasts for hierarchical time series are distributed in 
the hierarchy using bottom-up, top-down, middle-out and optimal combination 
methods. Three top-down methods are available: the two Gross-Sohl methods and 
the forecast-proportion approach of Hyndman, Ahmed, and Athanasopoulos (2011).

## Installation
You can install the **stable** version on 
[R CRAN](http://cran.r-project.org/package=hts).

```s
install.packages('hts', dependencies = TRUE)
```

You can also install the **development** version from
[Github](https://github.com/robjhyndman/gts)

```s
# install.packages("devtools")
library(devtools)
install_github("hts", "robjhyndman") 
```

## Usage

```s
# library(hts)
demo(htseg1)  # hts example 1
demo(htseg2)  # hts example 2
demo(infantgts)  # gts
```

## License

This package is free and open source software, licensed under GPL (>= 2).

# hts
[![Travis-CI Build Status](https://travis-ci.org/robjhyndman/hts.svg?branch=master)](https://travis-ci.org/robjhyndman/hts)
[![Coverage Status](https://coveralls.io/repos/github/robjhyndman/hts/badge.svg?branch=master)](https://coveralls.io/github/robjhyndman/hts?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/hts)](https://cran.r-project.org/package=hts)
[![Downloads](http://cranlogs.r-pkg.org/badges/hts)](https://cran.r-project.org/package=hts)
[![Pending Pull-Requests](http://githubbadges.herokuapp.com/robjhyndman/hts/pulls.svg?style=flat)](https://github.com/robjhyndman/hts/pulls)


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
[R CRAN](https://cran.r-project.org/package=hts).

```s
install.packages('hts', dependencies = TRUE)
```

You can also install the **development** version from
[Github](https://github.com/robjhyndman/gts)

```s
# install.packages("devtools")
devtools::install_github("robjhyndman/hts")
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

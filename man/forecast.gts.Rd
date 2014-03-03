\name{forecast.gts}
\Rdversion{1.1}
\alias{forecast.gts}
\title{Forecast a hierarchical or grouped time series}
\description{
Methods for forecasting hierarchical or grouped time series.
}
\usage{
\S3method{forecast}{gts}(object, h = ifelse(frequency(object) > 1L,
                         2L * frequency(object), 10L), 
                         method = c("comb", "bu", "mo", 
                                    "tdgsa", "tdgsf", "tdfp"),
                         fmethod = c("ets", "arima", "rw"), 
                         keep.fitted = FALSE, keep.resid = FALSE,
                         positive = FALSE, lambda = NULL, level, 
                         weights = c("none", "sd", "nseries"),
                         parallel = FALSE, num.cores = 2,
                         xreg = NULL, newxreg = NULL, ...) 
}
\arguments{
  \item{object}{Hierarchical or grouped time series object of class \code{{gts}}}
  \item{h}{Forecast horizon}
  \item{method}{Method for distributing forecasts within the hierarchy. See details}
  \item{fmethod}{Forecasting method to use}
  \item{keep.fitted}{If TRUE, keep fitted values at the bottom level.}
  \item{keep.resid}{If TRUE, keep residuals at the bottom level.}
  \item{positive}{If TRUE, forecasts are forced to be strictly positive}
  \item{lambda}{Box-Cox transformation parameter}
  \item{weights}{Weights used for "optimal combination" method. When \code{weights = "sd"},
  it takes account of the standard deviation of forecasts;
  when \code{weights} = "nseries", weights are equal to the inverse of row sums of the summing matrix.}
  \item{level}{Level used for "middle-out" method (only used when \code{method="mo"})}
  \item{parallel}{If TRUE, import \code{parallel} package to allow parallel processing}
  \item{num.cores}{If parallel = TRUE, specify how many cores are going to be used}
  \item{xreg}{When \code{fmethod = "arima"}, a vector or matrix of external regressors, which must have the same number of rows as the original univariate time series}
  \item{newxreg}{When \code{fmethod = "arima"}, a vector or matrix of external regressors, which must have the same number of rows as the original univariate time series}
  \item{...}{Other arguments passing to \code{\link[forecast]{ets}} or \code{\link[forecast]{auto.arima}}}
}
\value{A forecasted hierarchical/grouped time series of class \code{gts}.}
\details{
Base methods implemented include ETS, ARIMA and the naive (random walk) models.
Forecasts are distributed in the hierarchy using bottom-up, top-down, middle-out and
optimal combination methods. Three top-down methods are available: the two Gross-Sohl
methods and the forecast-proportion approach of Hyndman, Ahmed, and Athanasopoulos (2011).
The "middle-out" method \code{"mo"} uses bottom-up (\code{"bu"}) for levels higher than \code{level}
and top-down forecast proportions (\code{"tdfp"}) for levels lower than \code{level}. For non-hierarchical grouped data, only bottom-up and combination methods are possible, as any method involving top-down disaggregation requires a hierarchical ordering of groups.
}
\references{
G. Athanasopoulos, R. A. Ahmed and R. J. Hyndman (2009) Hierarchical forecasts for Australian domestic tourism,
\emph{International Journal of Forecasting}, \bold{25}, 146-166.

R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang (2011)
Optimal combination forecasts for hierarchical time series.
\emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
\url{http://robjhyndman.com/papers/hierarchical/}

Gross, C. and Sohl, J. (1990) Dissagregation methods to expedite product line forecasting,
\emph{Journal of Forecasting}, \bold{9}, 233-254.
}

\author{Earo Wang and Rob J Hyndman}

\seealso{
 \code{\link[hts]{hts}}, \code{\link[hts]{gts}}, \code{\link[hts]{plot.gts}}, \code{\link[hts]{accuracy.gts}}
}
\examples{
forecast(htseg1, h = 10, method = "bu", fmethod = "arima")
}
\keyword{ts}


\name{accuracy.gts}
\Rdversion{1.1}
\alias{accuracy.gts}
\title{
In-sample or out-of-sample accuracy measures for forecast grouped and hierarchical model
}
\description{
Returns a range of summary measures of the forecast accuracy. The function measures 
out-of-sample forecast accuracy based on (holdout data - forecasts) and in-sample 
accuracy at the bottom level when setting \code{keep.fitted = TRUE} in the 
\code{\link[hts]{forecast.gts}}. All measures are defined and discussed in Hyndman 
and Koehler (2006). 
}
\usage{	
accuracy.gts(fcasts, test, levels)
}
\arguments{
  \item{fcasts}{An object of class \code{gts}, containing the forecasted hierarchical or grouped
    time series. In-sample accuracy at the bottom level returns when \code{test}
  is missing.}
  \item{test}{An object of class \code{gts}, containing the holdout hierarchical time series}
  
  \item{levels}{Return the specified level(s), when carrying out 
  out-of-sample accuracy.} 
}
\value{
Matrix giving forecast accuracy measures.
\item{ME}{Mean Error}
\item{RMSE}{Root Mean Square Error}
\item{MAE}{Mean Absolute Error}
\item{MAPE}{Mean Absolute Percentage Error}
\item{MPE}{Mean Percentage Error}
\item{MASE}{Mean Absolute Scaled Error}
}
\details{
  MASE calculation is scaled using MAE of in-sample naive forecasts for non-seasonal time series, and in-sample seasonal naive forecasts for seasonal time series.
}
\references{
R. J. Hyndman and A. Koehler (2006), Another look at measures of forecast accuracy, \emph{International Journal of Forecasting}, \bold{22}, 679-688.
}
\author{
Rob J Hyndman and Earo Wang
}
\seealso{
\code{\link[hts]{hts}}, \code{\link[hts]{plot.gts}}, \code{\link[hts]{forecast.gts}}, \code{\link[forecast]{accuracy}}
}
\examples{
data <- window(htseg2, start = 1992, end = 2002)
test <- window(htseg2, start = 2003)
fcasts <- forecast(data, h = 5, method = "bu")
accuracy.gts(fcasts, test)
accuracy.gts(fcasts, test, levels = 1)
}
\keyword{error}


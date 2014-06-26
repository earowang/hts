\name{aggts}
\alias{aggts}
\title{Extract selected time series from a gts object}
\description{
The time series from selected levels of a hierarchical/grouped time series or a forecasted
hierarchical/grouped time series are returned as a multivariate time series.
}
\usage{
aggts(y, levels, forecasts = TRUE)
}

\arguments{
  \item{y}{An object of class \code{{gts}}.}
  \item{levels}{Integer(s) or string(s) giving the specified level(s).}
  \item{forecasts}{If \code{y} contains forecasts and historical data, then \code{forecasts} 
    indicates whether to return the forecasts or the historical data. Otherwise it is ignored.}
}
\author{Earo Wang}

\seealso{\code{\link[hts]{allts}}}

\examples{
aggts(htseg1, levels = c(0, 2))
aggts(infantgts, levels = "State")
}
\keyword{ts}

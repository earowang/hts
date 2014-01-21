\name{aggts}
\alias{aggts}
\title{Extract selected time series from a gts object}
\description{
The time series from selected levels of a hierarchical/grouped time series or a forecasted
hierarchical/grouped time series are returned as a multivariate time series.
}
\usage{
aggts(y, levels, forecast = TRUE)
}

\arguments{
  \item{y}{An object of class \code{{gts}}.}
  \item{levels}{Return the specified level(s).}
  \item{forecast}{If \code{obj} contains forecasts and historical data, then \code{forecasts} indicates whether to return the forecasts or the historical data. Otherwise it is ignored.}
}
\author{Earo Wang}

\examples{
aggts(htseg1)
aggts(htseg1, levels = c(0, 2))
}
\keyword{ts}

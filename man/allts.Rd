\name{allts}
\alias{allts}
\title{Extract all time series from a gts object}
\description{
The time series from all levels of a hierarchical/grouped time series or a forecasted
hierarchical/grouped time series are returned as a multivariate time series.
}
\usage{
allts(y, forecasts = TRUE)
}

\arguments{
  \item{y}{An object of class \code{\link[hts]{gts}}.}
  \item{forecasts}{If \code{y} contains forecasts and historical data, then \code{forecasts} indicates whether to return the forecasts or the historical data. Otherwise it is ignored.}
}
\author{Rob J Hyndman}

\seealso{\code{\link[hts]{aggts}}}

\examples{
allts(htseg1)
}
\keyword{ts}

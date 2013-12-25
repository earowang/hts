\name{window.gts}
\Rdversion{1.1}
\alias{window.gts}
\title{Time window of a gts object}
\description{
Extracts a subset of the time series from a grouped time series object.
}
\usage{
\S3method{window}{gts}(x, ...)
}

\arguments{
  \item{x}{An object of class \code{\link[hts]{gts}}.}
  \item{...}{All other arguments are passed to \code{\link[stats]{window.ts}}.}
}
\author{Rob J Hyndman}

\examples{
window(htseg2, start = 2000, end = 2001)
}
\keyword{ts}

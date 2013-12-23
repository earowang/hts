\name{summary.gts}
\Rdversion{1.1}
\alias{summary.gts}
\title{Print a hierarchical or grouped time series in detail}
\description{
Display hierarchical or grouped time series in detail on the screen.  
}
\usage{
\S3method{summary}{gts}(object, ...)
}
\arguments{
  \item{object}{A hierarchical or grouped time series}
  \item{...}{Other arguments passing to \code{summary} function}
}
\value{
Display hierarchical or grouped time series in detail on the screen.  
}
\author{Yiru Wang}

\seealso{
\code{\link[hts]{accuracy.gts}}, \code{\link[hts]{forecast.gts}}, \code{\link[hts]{plot.gts}}
}

\examples{
x <- forecast(htseg1, method = "bu", keep.fitted = TRUE)
summary(x)
}

\keyword{ts}

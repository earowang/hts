\name{print.gts}
\Rdversion{1.1}
\alias{print.gts}
\title{Print a hierarchical or grouped time series}
\description{
Display hierarchical or grouped time series on the screen.  
}
\usage{
\S3method{print}{gts}(x, ...)
}
\arguments{
  \item{x}{A hierarchical or grouped time series}
  \item{...}{Other arguments passing to \code{print} function}
}
\value{
Display hierarchical or grouped time series on the screen.  
}
\author{Earo Wang}

\seealso{
\code{\link[hts]{accuracy.gts}}, \code{\link[hts]{forecast.gts}}, \code{\link[hts]{plot.gts}}
}

\examples{
print(htseg1)
}

\keyword{ts}

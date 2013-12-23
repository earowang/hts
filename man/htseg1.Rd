\name{htseg1}
\Rdversion{1.1}
\alias{htseg1}
\alias{htseg2}
\docType{data}
\title{
Simple examples of hierarchical time series.
}
\description{These are simulated data. \code{htseg1} has three levels with a total of 8 series each of length 10.
\code{htseg2} has four levels with a total of 17 series each of length 16.
}
\usage{data(htseg1)
data(htseg2)}
\format{
Objects of class \code{\link[hts]{hts}}.
}
\references{
R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang (2011) 
Optimal combination forecasts for hierarchical time series. 
\emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
\url{http://robjhyndman.com/papers/hierarchical/}
}
\examples{
plot(htseg1)
}
\keyword{datasets}

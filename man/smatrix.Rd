\name{smatrix}
\Rdversion{1.1}
\alias{smatrix}
\title{Summing matrix for hierarchical or grouped time series}
\description{
This function returns the summing matrix for a hierarchical or grouped time series, as defined in Hyndman et al. (2011).
}
\usage{
smatrix(xts)
}
\arguments{
  \item{xts}{Hierarchical or grouped time series of class \code{gts}.}
}
\value{A numerical matrix.}
\references{
R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang (2011) 
Optimal combination forecasts for hierarchical time series. 
\emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
\url{http://robjhyndman.com/papers/hierarchical/}
}

\author{Earo Wang}

\seealso{
 \code{\link[hts]{hts}}, \code{\link[hts]{gts}}, \code{\link[hts]{combinef}}
}

\examples{
smatrix(htseg1)
}
\keyword{ts}


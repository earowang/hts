\name{infantgts}
\alias{infantgts}
\docType{data}
\title{
Regional infant mortality counts across Australia from 1933 to 2003.
}
\description{
These are infant mortality counts. This data set is an example of \code{gts}, where the total infant mortality count in Australia can be first disaggregated by sex then by state, or vice versa. 
}
\usage{data(infantgts)}
\format{
Objects of class \code{\link[hts]{gts}}.
}
\references{
R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang (2011) 
Optimal combination forecasts for hierarchical time series. 
\emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
}
\examples{
plot(infantgts)
}
\keyword{datasets}

\name{combinef}
\Rdversion{1.1}
\alias{combinef}
\title{Optimally combine forecasts from a hierarchical or grouped time series}
\description{
Using the method of Hyndman et al. (2011), this function optimally combines the forecasts at all levels of a hierarchical time series. The \code{\link{forecast.gts}} calls this function when the \code{comb} method is selected.
}
\usage{
combinef(fcasts, nodes, weights = NULL)
}
\arguments{
  \item{fcasts}{Matrix of forecasts for all levels of the hierarchical time series. Each row represents one forecast horizon and each column represents one time series from the hierarchy.}
  \item{nodes}{A list contains the number of child nodes referring to \code{hts}, when the object class is \code{hts}.
    The summing matrix defines how the forecasts at the bottom level are summed to obtain each time series in the group matrix.}
  \item{weights}{A numeric vector.}
}
\value{Only return the reconciled forecasts at the bottom level.}
\references{
R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang (2011) 
Optimal combination forecasts for hierarchical time series. 
\emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
\url{http://robjhyndman.com/papers/hierarchical/}
}

\author{Rob J Hyndman and Roman A Ahmed}

\seealso{
 \code{\link[hts]{hts}}, \code{\link[hts]{forecast.gts}}
}
\keyword{ts}

\examples{
h <- 12 
ally <- aggts(htseg1)
allf <- matrix(NA, nrow = h, ncol = ncol(ally))
for(i in 1:ncol(ally))
	allf[,i] <- forecast(auto.arima(ally[,i]), h=h)$mean
allf <- ts(allf, start = 51)
class(allf) <- "hts"
y.f <- combinef(allf, htseg1$nodes, weights = NULL)
}

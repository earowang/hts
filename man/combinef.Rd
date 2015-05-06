\name{combinef}
\Rdversion{1.1}
\alias{combinef}
\title{Optimally combine forecasts from a hierarchical or grouped time series}
\description{
Using the method of Hyndman et al. (2011), this function optimally combines the forecasts at all levels of a hierarchical time series. The \code{\link{forecast.gts}} calls this function when the \code{comb} method is selected.
}
\usage{
combinef(fcasts, nodes, groups, weights = NULL, 
         algorithms = c("lu", "cg", "chol", "recursive", "slm"),
         keep = c("gts", "all", "bottom"))
}
\arguments{
  \item{fcasts}{Matrix of forecasts for all levels of the hierarchical time series. Each row represents one forecast horizon and each column represents one time series from the hierarchy.}
  \item{nodes}{If the object class is \code{hts}, a list contains the number of child nodes referring to \code{hts}.}
  \item{groups}{If the object class is \code{gts}, a gmatrix is required, which is the same as \code{groups} in the function \code{gts}.}
  \item{weights}{A numeric vector. The default is \code{NULL} that means the ordinary least squares is implemented.}
  \item{algorithms}{An algorithm to be used for computing reconciled forecasts. See details.}
  \item{keep}{Return a \code{gts} object or the the reconciled forecasts at the bottom level.}
}
\value{Return the reconciled \code{gts} object or forecasts at the bottom level.}

\references{
R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang (2011) 
Optimal combination forecasts for hierarchical time series. 
\emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
\url{http://robjhyndman.com/papers/hierarchical/}

Hyndman, R. J., Lee, A., & Wang, E. (2014). 
Fast computation of reconciled forecasts for hierarchical and grouped time series. 
\emph{Working paper 17/14, Department of Econometrics & Business Statistics, Monash University.}
\url{http://robjhyndman.com/working-papers/hgts/}
}

\author{Alan Lee, Rob J Hyndman and Earo Wang}

\seealso{
 \code{\link[hts]{hts}}, \code{\link[hts]{forecast.gts}}
}
\keyword{ts}

\examples{
# hts example
\dontrun{h <- 12 
ally <- aggts(htseg1)
allf <- matrix(NA, nrow = h, ncol = ncol(ally))
for(i in 1:ncol(ally))
	allf[,i] <- forecast(auto.arima(ally[,i]), h = h, PI = FALSE)$mean
allf <- ts(allf, start = 51)
y.f <- combinef(allf, htseg1$nodes, weights = NULL, keep = "gts", algorithms = "lu")
plot(y.f)}

# gts example
\dontrun{abc <- ts(5 + matrix(sort(rnorm(200)), ncol = 4, nrow = 50))
g <- rbind(c(1,1,2,2), c(1,2,1,2))
y <- gts(abc, groups = g)
h <- 12
ally <- aggts(y)
allf <- matrix(NA,nrow = h,ncol = ncol(ally))
for(i in 1:ncol(ally))
  allf[,i] <- forecast(auto.arima(ally[,i]),h = h, PI = FALSE)$mean
allf <- ts(allf, start = 51)
y.f <- combinef(allf, groups = g, keep ="gts", algorithms = "lu")
plot(y.f)}
}

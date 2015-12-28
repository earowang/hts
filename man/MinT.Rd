\name{MinT}
\Rdversion{1.1}
\alias{MinT}
\title{Trace minimization for hierarchical or grouped time series}
\description{
  Using the method of Wickramasuriya et al. (2015), this function combines the forecasts at all levels of a hierarchical or grouped time series.
}
\usage{
  MinT(fcasts, nodes, groups, residual, covariance = c("sam", "shr"),
       algorithms = c("lu", "cg", "chol"), keep = c("gts", "all", "bottom")) 
}
\arguments{
  \item{fcasts}{Matrix of forecasts for all levels of the hierarchical time series. Each row represents one forecast horizon and each column represents one time series from the hierarchy}
  \item{nodes}{If the object class is hts, a list contains the number of child nodes referring to hts.}
  \item{groups}{If the object is gts, a gmatrix is required, which is the same as groups in the function gts.}
  \item{residuals}{Matrix of insample residuals for all time series in the hierarchy. Each column referring to one time series.}
  \item{Covariance}{Type of the covariance matrix to be used. Sample covariance matrix ("sam") or shrinking towards a diagonal unequal variances ("shr").}
  \item{algorithms}{Algorithm used to compute inverse of the matrices.}
  \item{keep}{Return a gts object or the reconciled forecasts at the bottom level.}
}
\value{Return the reconciled \code{gts} object or forecasts at the bottom level.}

\references{
  Wickramasuriya, S. L., Athanasopoulos, G., & Hyndman, R. J. (2015). 
  Forecasting hierarchical and grouped time series through trace minimization. 
  \emph{Working paper 17/14, Department of Econometrics & Business Statistics, Monash University.}
  \url{http://robjhyndman.com/working-papers/mint/}
  
  Hyndman, R. J., Lee, A., & Wang, E. (2014). 
  Fast computation of reconciled forecasts for hierarchical and grouped time series. 
  \emph{Working paper 15/15, Department of Econometrics & Business Statistics, Monash University.}
  \url{http://robjhyndman.com/working-papers/hgts/}
}

\author{Shanika L Wickramasuriya}

\seealso{
  \code{\link[hts]{hts}}, \code{\link[hts]{gts}}, \code{\link[hts]{combinef}}
}
\keyword{ts}

\examples{
# hts example
\dontrun{h <- 12 
ally <- aggts(htseg1)
n <- nrow(ally)
p <- ncol(ally)
allf <- matrix(NA, nrow = h, ncol = p)
res <- matrix(NA, nrow = n, ncol = p)
for(i in 1:p)
{
  fit <- auto.arima(ally[, i])
  allf[, i] <- forecast(fit, h = h)$mean
  res[, i] <- na.omit(ally[, i] - fitted(fit))
}
allf <- ts(allf, start = 51)
y.f <- MinT(allf, htseg1$nodes, residual = res, covariance = "shr", keep = "gts", algorithms = "lu")
plot(y.f)}
  
# gts example
\dontrun{abc <- ts(5 + matrix(sort(rnorm(200)), ncol = 4, nrow = 50))
g <- rbind(c(1,1,2,2), c(1,2,1,2))
y <- gts(abc, groups = g)
h <- 12
ally <- aggts(y)
n <- nrow(ally)
p <- ncol(ally)
allf <- matrix(NA,nrow = h,ncol = ncol(ally))
res <- matrix(NA, nrow = n, ncol = p)
for(i in 1:p)
{
  fit <- auto.arima(ally[, i])
  allf[, i] <- forecast(fit, h = h)$mean
  res[, i] <- na.omit(ally[, i] - fitted(fit))
}
allf <- ts(allf, start = 51)
y.f <- MinT(allf, groups = g, residual = res, covariance = "shr", keep = "gts", algorithms = "lu")
plot(y.f)}
}

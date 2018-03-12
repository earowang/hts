

#' Hierarchical and grouped time series
#' 
#' This package presents functions to create, plot and forecast hierarchical
#' and grouped time series. In forecasting hierarchical and grouped time
#' series, the base methods implemented include ETS, ARIMA and the naive
#' (random walk) models. Forecasts for grouped time series are calibrated using
#' bottom-up and optimal combination methods. Forecasts for hierarchical time
#' series are distributed in the hierarchy using bottom-up, top-down,
#' middle-out and optimal combination methods. Three top-down methods are
#' available: the two Gross-Sohl methods and the forecast-proportion approach
#' of Hyndman, Ahmed, and Athanasopoulos (2011).
#' 
#' 
#' @name hts-package
#' @docType package
#' @author Rob J Hyndman, Alan Lee, Earo Wang and Shanika L Wickramasuriya with 
#' contributions from Roman A Ahmed and Han Lin Shang to earlier versions of the 
#' package
#' 
#' @references G. Athanasopoulos, R. A. Ahmed and R. J. Hyndman (2009)
#' Hierarchical forecasts for Australian domestic tourism, \emph{International
#' Journal of Forecasting}, \bold{25}, 146-166.
#' 
#' R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang (2011) Optimal
#' combination forecasts for hierarchical time series.  \emph{Computational
#' Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
#' \url{http://robjhyndman.com/papers/hierarchical/}
#' 
#' Hyndman, R. J., Lee, A., & Wang, E. (2014).  Fast computation of reconciled
#' forecasts for hierarchical and grouped time series.  \emph{Working paper
#' 17/14, Department of Econometrics & Business Statistics, Monash University.}
#' \url{http://robjhyndman.com/working-papers/hgts/}
#' Wickramasuriya, S. L., Athanasopoulos, G., & Hyndman, R. J. (2015). 
#' Forecasting hierarchical and grouped time series through trace minimization. 
#' \emph{Working paper 15/15, Department of Econometrics & Business Statistics, 
#' Monash University.} \url{http://robjhyndman.com/working-papers/mint/}
#' @keywords package
NULL





#' Simple examples of hierarchical time series.
#' 
#' These are simulated data. \code{htseg1} has three levels with a total of 8
#' series each of length 10. \code{htseg2} has four levels with a total of 17
#' series each of length 16.
#' 
#' 
#' @name htseg1
#' @aliases htseg1 htseg2
#' @docType data
#' @format Objects of class \code{\link[hts]{hts}}.
#' @references R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang
#' (2011) Optimal combination forecasts for hierarchical time series.
#' \emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
#' \url{http://robjhyndman.com/papers/hierarchical/}
#' @keywords datasets
#' @examples
#' 
#' plot(htseg1)
#' 
NULL





#' Regional infant mortality counts across Australia from 1933 to 2003.
#' 
#' These are infant mortality counts. This data set is an example of
#' \code{gts}, where the total infant mortality count in Australia can be first
#' disaggregated by sex then by state, or vice versa.
#' 
#' 
#' @name infantgts
#' @docType data
#' @format Objects of class \code{\link[hts]{gts}}.
#' @references R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang
#' (2011) Optimal combination forecasts for hierarchical time series.
#' \emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
#' @keywords datasets
#' @examples
#' 
#' plot(infantgts)
#' 
NULL




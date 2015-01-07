\name{hts}
\Rdversion{1.1}
\alias{hts}
\title{Create a hierarchical time series}
\description{
Method for creating hierarchical time series.  
}
\usage{
hts(y, nodes, bnames = colnames(y), characters)
}
\arguments{
  \item{y}{A matrix or multivariate time series contain the bottom level series.}
  \item{nodes}{A list contains the number of child nodes associated with each level,
  which indicates the hierarchical structure. The default is
  a simple hierarchy with only 2 levels (i.e. total and bottom). If the argument
  \code{characters} is used, \code{nodes} will be automatically generated within the function.}
  \item{bnames}{The names of the bottom time series.}
  \item{characters}{Integers indicate the segments in which the bottom level names
  can be read in order to construct the corresponding node structure and its labels.
  For instance, suppose one of the bottom series is named "VICMelb" referring to the city 
  of Melbourne within the state of Victoria. Then \code{characters} would
  be specified as \code{c(3, 4)} referring to states of 3 characters (e.g., "VIC") and 
  cities of 4 characters (e.g., "Melb") All the bottom names must be of the same length,
  with number of characters for each segment the same for all series.}
}
\value{
  \item{bts}{Multivariate time series containing the bottom level series}
  \item{nodes}{Information about the nodes of a hierarchical time series}
  \item{labels}{Information about the labels that are used for plotting.}
}
\references{
R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang (2011) 
Optimal combination forecasts for hierarchical time series. 
\emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
\url{http://robjhyndman.com/papers/hierarchical/}
}
\author{Earo Wang and Rob J Hyndman}

\seealso{\code{\link[hts]{gts}},
\code{\link[hts]{accuracy.gts}}, \code{\link[hts]{forecast.gts}}, \code{\link[hts]{plot.gts}}
}

\examples{
# Example 1
# The hierarchical structure looks like 2 child nodes associated with level 1,
# which are followed by 3 and 2 sub-child nodes respectively at level 2.
nodes <- list(2, c(3, 2))
abc <- ts(5 + matrix(sort(rnorm(500)), ncol = 5, nrow = 100))
x <- hts(abc, nodes)

# Example 2
# Suppose we've got the bottom names that can be useful for constructing the node
# structure and the labels at higher levels. We need to specify how to split them 
# in the argument "characters".
library(hts)
abc <- ts(5 + matrix(sort(rnorm(1000)), ncol = 10, nrow = 100))
colnames(abc) <- c("A10A", "A10B", "A10C", "A20A", "A20B",
                   "B30A", "B30B", "B30C", "B40A", "B40B")
y <- hts(abc, characters = c(1, 2, 1))
}

\keyword{ts}

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
  \item{nodes}{A list contains the number of child nodes for each level except
  for the bottom one. It indicates the hierarchical structure. The default is
  a simple hierarchy with only 2 levels (i.e. total and bottom).}
  \item{bnames}{The names of the bottom time series.}
  \item{characters}{A set of integers indicate how to split the \code{bnames}
  in order to construct labels.}
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

\seealso{
\code{\link[hts]{accuracy.gts}}, \code{\link[hts]{forecast.gts}}, \code{\link[hts]{plot.gts}}
}

\examples{
# Example 1
# The hierarchical structure looks like one root node associated with 2 child
# nodes. These 2 child nodes have 3 and 2 sub-child nodes respectively.
nodes <- list(2, c(3, 2))
abc <- ts(5 + matrix(sort(rnorm(500)), ncol = 5, nrow = 100))
x <- hts(abc, nodes)

# Example 2
# Suppose we've got the bottom names that can be useful for constructing labels
# at higher levels. We need to specify how to split them in the argument
# "characters".
nodes <- list(2, c(2, 2), c(3, 2, 3, 2))
abc <- ts(5 + matrix(sort(rnorm(1000)), ncol = 10, nrow = 100))
colnames(abc) <- c("A10A", "A10B", "A10C", "A20A", "A20B",
                   "B30A", "B30B", "B40A", "B40B", "B40C")
y <- hts(abc, nodes, characters = c(1, 2, 1))
}


\keyword{ts}

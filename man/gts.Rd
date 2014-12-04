\name{gts}
\Rdversion{1.1}
\alias{gts}
\alias{print.gts}
\alias{summary.gts}
\title{Create a grouped time series}
\description{
Method for creating grouped time series. 
}
\usage{
gts(y, groups, gnames = rownames(groups), characters)
}
\arguments{
  \item{y}{A matrix or multivariate time series contains the bottom level series.}
  \item{groups}{Group matrix indicates the group structure, with one column for
  each series when completely disaggregated, and one row for each grouping of the 
  time series. It allows either a numerical matrix or a matrix consisting of strings
  that can be used for labelling. If the argument \code{characters} is used, then 
  \code{groups} will be automatically generated within the function.}
  \item{gnames}{Specify the group names.}
  \item{characters}{A vector of integers, or a list containing vectors of integers, 
  indicating the segments in which bottom level names can be read in order to construct 
  the corresponding grouping matrix and its labels. A \code{list} class is used when a 
  grouped time series includes one or more hierarchies. For example, a grouped time 
  series may involve a geographical grouping and a product grouping, with each of them 
  associated with a 2-level hierarchy. In this situation, a bottom level name such as
  "VICMelbAB" would indicate the state "VIC" (3 characters) followed by the city "Melb" 
  (4 characters), then the product category "A" (1 character) followed by the sub-product 
  category "B" (1 character). In this example, the specification of \code{characters} is 
  \code{list(c(3, 4), c(1, 1))}, where the first element \code{c(3, 4)} corresponds to 
  the geographical hierarchy and the second element corresponds to the product hierarchy. 
  In the special case where there is a non-hierarchical grouped time series, a vector
  of integers is also possible. For example, a grouped time series may involve state, age and
  sex grouping variables. In this situation, a bottom level name such as "VIC1F" would indicate
  the state "VIC", age group "1" and sex "F". Because none of these is hierarchical, we could 
  specify \code{characters = list(3, 1, 1)}, or as a simple numeric vector:
  \code{characters = c(3, 1, 1)}. This implies its non-hierarchical structure and its 
  characters segments. Again, all bottom level names must be of the same length. Currently,
  the use of \code{characters} only supports 2-way cross-products for grouping variables.
  Specifying \code{groups} is more general (but more complicated), as any combination of 
  grouping variables can be used.}
}
\value{
  \item{bts}{Multivariate time series contains the bottom level series}
  \item{groups}{Information about the groups of a grouped time series}
  \item{labels}{Information about the labels that are used for plotting.}
}
\references{
R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang (2011) 
Optimal combination forecasts for hierarchical time series. 
\emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
\url{http://robjhyndman.com/papers/hierarchical/}
}
\author{Earo Wang and Rob J Hyndman}

\seealso{\code{\link[hts]{hts}},
\code{\link[hts]{accuracy.gts}}, \code{\link[hts]{forecast.gts}}, \code{\link[hts]{plot.gts}}
}

\examples{
# Example 1 illustrating the usage of the "groups" argument
abc <- ts(5 + matrix(sort(rnorm(1600)), ncol = 16, nrow = 100))
sex <- rep(c("female", "male"), each = 8)
state <- rep(c("NSW", "VIC", "QLD", "SA", "WA", "NT", "ACT", "TAS"), 2)
gc <- rbind(sex, state)  # a matrix consists of strings.
gn <- rbind(rep(1:2, each = 8), rep(1:8, 2))  # a numerical matrix
rownames(gc) <- rownames(gn) <- c("Sex", "State")
x <- gts(abc, groups = gc)
y <- gts(abc, groups = gn)

# Example 2 with two simple hierarchies (geography and product) to show the argument "characters"
bnames1 <- c("VICMelbAA", "VICMelbAB", "VICGeelAA", "VICGeelAB",  
             "VICMelbBA", "VICMelbBB", "VICGeelBA", "VICGeelBB",
             "NSWSyndAA", "NSWSyndAB", "NSWWollAA", "NSWWollAB", 
             "NSWSyndBA", "NSWSyndBB", "NSWWollBA", "NSWWollBB")
bts1 <- matrix(ts(rnorm(160)), ncol = 16)
colnames(bts1) <- bnames1
x1 <- gts(bts1, characters = list(c(3, 4), c(1, 1)))

# Example 3 with a non-hierarchical grouped time series of 3 grouping variables (state, age and sex)
bnames2 <- c("VIC1F", "VIC1M", "VIC2F", "VIC2M", "VIC3F", "VIC3M",
             "NSW1F", "NSW1M", "NSW2F", "NSW2M", "NSW3F", "NSW3M")
bts2 <- matrix(ts(rnorm(120)), ncol = 12)
colnames(bts2) <- bnames2
x2 <- gts(bts2, characters = c(3, 1, 1))
}

\keyword{ts}

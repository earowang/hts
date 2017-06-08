#' Simple examples of hierarchical time series 
#'
#' These are simulated data. \code{htseg1} has three levels with a total of 8
#' series each of length 10. \code{htseg2} has four levels with a total of 17
#' series each of length 16.
#'
#' @format Objects of class \code{\link[hts]{hts}}.
#' @docType data
#' @name htseg1
#' @aliases htseg1 htseg2
#' @examples
#' plot(htseg1)
NULL

#' Regional infant mortality counts across Australia from 1933 to 2003.
#'
#' These are infant mortality counts. This data set is an example of
#' \code{gts}, where the total infant mortality count in Australia can be first
#' disaggregated by sex then by state, or vice versa.
#'
#' @format Objects of class \code{\link[hts]{gts}}.
#' @docType data
#' @name infantgts
#' @examples
#' plot(infantgts)
NULL

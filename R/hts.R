#' Create a hierarchical time series
#' 
#' Method for creating hierarchical time series.
#' 
#' 
#' @rdname hts-class
#' @param y A matrix or multivariate time series contain the bottom level
#' series.
#' @param nodes A list contains the number of child nodes associated with each
#' level, which indicates the hierarchical structure. The default is a simple
#' hierarchy with only 2 levels (i.e. total and bottom). If the argument
#' \code{characters} is used, \code{nodes} will be automatically generated
#' within the function.
#' @param bnames The names of the bottom time series.
#' @param characters Integers indicate the segments in which the bottom level
#' names can be read in order to construct the corresponding node structure and
#' its labels.  For instance, suppose one of the bottom series is named
#' "VICMelb" referring to the city of Melbourne within the state of Victoria.
#' Then \code{characters} would be specified as \code{c(3, 4)} referring to
#' states of 3 characters (e.g., "VIC") and cities of 4 characters (e.g.,
#' "Melb") All the bottom names must be of the same length, with number of
#' characters for each segment the same for all series.
#' @param ... Extra arguments passed to \code{print} and \code{summary}.
#' @return \item{bts}{Multivariate time series containing the bottom level
#' series} \item{nodes}{Information about the nodes of a hierarchical time
#' series} \item{labels}{Information about the labels that are used for
#' plotting.}
#' @author Earo Wang and Rob J Hyndman
#' @seealso \code{\link[hts]{gts}}, \code{\link[hts]{accuracy.gts}},
#' \code{\link[hts]{forecast.gts}}, \code{\link[hts]{plot.gts}}
#' @references Hyndman, R. J., Ahmed, R. A., Athanasopoulos, G., & Shang, H. L.
#' (2011). Optimal combination forecasts for hierarchical time series.
#' \emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
#' \url{http://robjhyndman.com/papers/hierarchical/}
#' @keywords ts
#' @examples
#' 
#' # Example 1
#' # The hierarchical structure looks like 2 child nodes associated with level 1,
#' # which are followed by 3 and 2 sub-child nodes respectively at level 2.
#' nodes <- list(2, c(3, 2))
#' abc <- ts(5 + matrix(sort(rnorm(500)), ncol = 5, nrow = 100))
#' x <- hts(abc, nodes)
#' 
#' # Example 2
#' # Suppose we've got the bottom names that can be useful for constructing the node
#' # structure and the labels at higher levels. We need to specify how to split them 
#' # in the argument "characters".
#' library(hts)
#' abc <- ts(5 + matrix(sort(rnorm(1000)), ncol = 10, nrow = 100))
#' colnames(abc) <- c("A10A", "A10B", "A10C", "A20A", "A20B",
#'                    "B30A", "B30B", "B30C", "B40A", "B40B")
#' y <- hts(abc, characters = c(1, 2, 1))
#' 
#' @export hts
hts <- function(y, nodes, bnames = colnames(y), characters) {
  # Construct the hierarchical time series.
  #
  # Args:
  #   y*: The bottom time series assigned by the user. Same lengths and no NA.
  #   nodes: A list contains the number of child nodes for each level except
  #     for the bottom one. If missing, it's assumed to have only one level.
  #   bnames: The names of the bottom time series.
  #   characters: Define how to split the "bnames" in order to construct the
  #     level labels. Otherwise, use the defaul labelling system. The arg also
  #     implies the node structure.
  #
  # Returns:
  #   A hierarchical time series.
  #
  # Error handling:
  if (!is.ts(y)) {
    y <- stats::as.ts(y)
  }
  nbts <- ncol(y)

  if (is.null(nbts) || nbts <= 1L) {
    stop("Argument y must be a multivariate time series.", call. = FALSE)
  }
  if (missing(characters)) { # Arg "characters" not specified
    message("Since argument characters are not specified, the default labelling system is used.")
    if (missing(nodes)) {
      nodes <- list(nbts)
    }
    if (!is.list(nodes)) {
      stop("Argument nodes must be a list.", call. = FALSE)
    }
    if (length(nodes[[1L]]) != 1L) {
      stop("The root node cannot be empty.", call. = FALSE)
    }
    if (sum(nodes[[length(nodes)]]) != nbts) {
      stop("The number of terminal nodes is not consistent with the number of bottom time series.", call. = FALSE)
    }
    if (length(nodes) > 1L) {
      for (i in 1L:(length(nodes) - 1L)) {
        if (sum(nodes[[i]]) != length(nodes[[i + 1]])) {
          error <- sprintf("The number of nodes for the level %i is not equal to the number of series of level %i.", i - 1L, i)
          stop(error, call. = FALSE)
        }
      }
    }

    # Construct the level labels
    if (is.null(bnames)) {
      labels <- HierName(nodes) # HierName() defined below
      colnames(y) <- unlist(labels[length(labels)])
    } else {  # Keep bts names if specified
      hn <- HierName(nodes)
      last.label <- paste("Level", length(nodes))
      b.list <- list(bnames)
      names(b.list) <- last.label
      labels <- c(hn[-length(hn)], b.list)
      # if (length(hn) == 1L) {  # In case of a simple hierarchy of 2 levels
      #   labels <- c(hn, b.list)
      # } else {
      #   labels <- c(hn[-length(hn)], b.list)
      # }
    }
  } else { # Specified "characters" automates the node structure
    if (!all(nchar(bnames)[1L] == nchar(bnames)[-1L])) {
      stop("The bottom names must be of the same length.", call. = FALSE)
    }
    if (any(nchar(bnames) != sum(characters))) {
      warning("The argument characters is not fully specified for the bottom names.")
    }
    c.nodes <- CreateNodes(bnames, characters)
    nodes <- c.nodes$nodes
    labels <- c.nodes$labels
    y <- y[, c.nodes$index]
  }

  # Obtain other information
  names(nodes) <- paste("Level", 1L:length(nodes))

  output <- structure(
    list(bts = y, nodes = nodes, labels = labels),
    class = c("hts", "gts")
  )
  return(output)
}

#' Get nodes/groups from an hts/gts object
#' 
#' @rdname helper-functions
#' @param y An hts or gts object
#' series.
#' @export
get_nodes <- function(y) {
  if(!is.hts(y)) stop("'y' must be an hts object.", call. = FALSE)
  return(y$nodes)
}


# A function to convert the nodes list to gmatrix
GmatrixH <- function(xlist) {
  l.xlist <- length(xlist)
  num.bts <- sum(xlist[[l.xlist]])
  nlist <- unlist(lapply(xlist, length))
  # Create an empty matrix to contain the gmatrix
  gmat <- matrix(, nrow = l.xlist, ncol = num.bts)
  # Insert the bottom level
  gmat[nrow(gmat), ] <- seq(1L, num.bts)
  # Insert the middle levels in the reverse order
  if (l.xlist > 1L) {
    repcount <- xlist[[l.xlist]]
    for (i in (l.xlist - 1L):1L) {
      gmat[i, ] <- rep(1L:nlist[i + 1], repcount)
      repcount <- rowsum(repcount, rep(1L:nlist[i], xlist[[i]]))
    }
  }
  # Insert the top level
  gmat <- rbind(rep(1L, num.bts), gmat)

  dimnames(gmat) <- list(paste("Level", 0L:(nrow(gmat) - 1L)), colnames(xlist))
  class(gmat) <- "gmatrix"
  return(gmat)
}


# A function to return the NO. of nodes at each level
Mnodes <- function(xlist) {
  m <- c(unlist(lapply(xlist, length)), sum(xlist[[length(xlist)]]))
  return(m)
}


# A function to get the inverse of row sums of Smatrix
InvS4h <- function(xlist) {
  gmat <- GmatrixH(xlist)
  uniq <- apply(gmat, 1, unique)
  len <- nrow(gmat)
  inv.s <- vector(length = len, mode = "list")
  for (i in 1L:len) {
    inv.s[[i]] <- sapply(uniq[[i]], function(x) length(gmat[i, gmat[i, ] == x]))
  }
  inv.s <- 1/unlist(inv.s)
  return(inv.s)
}


# A function to set the default hierarchical names
HierName <- function(xlist) {
  l.xlist <- length(xlist)
  names.list <- list(length = l.xlist)
  names.list[[1L]] <- LETTERS[1L:xlist[[1L]]]
  if (l.xlist > 1L) {
    for (i in 2L:l.xlist) {
      # Grab the individual letters at each level
      ind <- unlist(sapply(xlist[[i]], function(x) LETTERS[1:x]))
      # Recursively paste
      names.list[[i]] <- paste0(rep(names.list[[i - 1]], xlist[[i]]), ind)
    }
    names(names.list) <- paste("Level", 1L:l.xlist)
  }
  names.list <- c("Level 0" = "Total", names.list)
  return(names.list)
}


# A function to create nodes based on segmentation of bottom names
# it also generate index for bottom time series
CreateNodes <- function(bnames, characters) {
  characters <- as.integer(characters)
  end <- cumsum(characters)
  start <- end - characters + 1L
  token <- sapply(end, function(x) substring(bnames, 1L, x))
  nc.token <- ncol(token)
  unique.str <- apply(token, 2, unique)
  nodes <- lapply(2L:nc.token, function(x) {
                    prefix <- substr(unique.str[[x]], start = 1L,
                                     stop = end[x - 1L])
                    return(table(prefix, dnn = NULL))
                      })
  nodes <- c(length(unique.str[[1L]]), nodes)
  # Construct labels based on characters
  names(unique.str) <- paste("Level", 1L:nc.token)
  extract.levels <- lapply(unique.str, function(x) levels(factor(x)))
  labels <- c("Level 0" = "Total", extract.levels)
  # Generate index for bottom time series
  idx <- match(extract.levels[[nc.token]], token[, nc.token])
  out <- list(nodes = nodes, labels = labels, index = idx)
  return(out)
}

#' @rdname hts-class
#' @param xts \code{hts} object.
#' @export
# A function to check whether it's the "hts" class.
is.hts <- function(xts) {
  is.element("hts", class(xts))
}

#' @rdname hts-class
#' @param x \code{hts} object.
#' @method print hts
#' @export
#' @export print.hts
# Print "hts" on the screen
print.hts <- function(x, ...) {
  mn <- Mnodes(x$nodes)
  cat("Hierarchical Time Series \n")
  cat(length(mn), "Levels \n")
  cat("Number of nodes at each level:", mn, "\n")
  cat("Total number of series:", sum(mn), "\n")

  if (is.null(x$histy)) {  # Original series
    cat("Number of observations per series:", nrow(x$bts), "\n")
    cat("Top level series: \n")
  } else {
    cat("Number of observations in each historical series:",
        nrow(x$histy), "\n")
    cat("Number of forecasts per series:", nrow(x$bts), "\n")
    cat("Top level series of forecasts: \n")
  }
  topts <- ts(rowSums(x$bts, na.rm = TRUE), start = stats::tsp(x$bts)[1L],
              frequency = stats::tsp(x$bts)[3L])
  print(topts)
}

#' @rdname hts-class
#' @param object \code{hts} object.
#' @method summary hts
#' @export
#' @export summary.hts
summary.hts <- function(object, ...) {
  NextMethod()
}

#' Create a grouped time series
#' 
#' Method for creating grouped time series.
#' 
#' @rdname gts-class
#' @aliases gts print.gts summary.gts
#' @param y A matrix or multivariate time series contains the bottom level
#' series.
#' @param groups Group matrix indicates the group structure, with one column
#' for each series when completely disaggregated, and one row for each grouping
#' of the time series. It allows either a numerical matrix or a matrix
#' consisting of strings that can be used for labelling. If the argument
#' \code{characters} is used, then \code{groups} will be automatically
#' generated within the function.
#' @param gnames Specify the group names.
#' @param characters A vector of integers, or a list containing vectors of
#' integers, indicating the segments in which bottom level names can be read in
#' order to construct the corresponding grouping matrix and its labels. A
#' \code{list} class is used when a grouped time series includes one or more
#' hierarchies. For example, a grouped time series may involve a geographical
#' grouping and a product grouping, with each of them associated with a 2-level
#' hierarchy. In this situation, a bottom level name such as "VICMelbAB" would
#' indicate the state "VIC" (3 characters) followed by the city "Melb" (4
#' characters), then the product category "A" (1 character) followed by the
#' sub-product category "B" (1 character). In this example, the specification
#' of \code{characters} is \code{list(c(3, 4), c(1, 1))}, where the first
#' element \code{c(3, 4)} corresponds to the geographical hierarchy and the
#' second element corresponds to the product hierarchy.  In the special case
#' where there is a non-hierarchical grouped time series, a vector of integers
#' is also possible. For example, a grouped time series may involve state, age
#' and sex grouping variables. In this situation, a bottom level name such as
#' "VIC1F" would indicate the state "VIC", age group "1" and sex "F". Because
#' none of these is hierarchical, we could specify \code{characters = list(3,
#' 1, 1)}, or as a simple numeric vector: \code{characters = c(3, 1, 1)}. This
#' implies its non-hierarchical structure and its characters segments. Again,
#' all bottom level names must be of the same length. Currently, the use of
#' \code{characters} only supports 2-way cross-products for grouping variables.
#' Specifying \code{groups} is more general (but more complicated), as any
#' combination of grouping variables can be used.
#' @param ... Extra arguments passed to \code{print} and \code{summary}.
#' @return \item{bts}{Multivariate time series contains the bottom level
#' series} \item{groups}{Information about the groups of a grouped time series}
#' \item{labels}{Information about the labels that are used for plotting.}
#' @author Earo Wang and Rob J Hyndman
#' @seealso \code{\link[hts]{hts}}, \code{\link[hts]{accuracy.gts}},
#' \code{\link[hts]{forecast.gts}}, \code{\link[hts]{plot.gts}}
#' @references R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang
#' (2011) Optimal combination forecasts for hierarchical time series.
#' \emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
#' \url{http://robjhyndman.com/papers/hierarchical/}
#' @keywords ts
#' @examples
#' 
#' # Example 1 illustrating the usage of the "groups" argument
#' abc <- ts(5 + matrix(sort(rnorm(1600)), ncol = 16, nrow = 100))
#' sex <- rep(c("female", "male"), each = 8)
#' state <- rep(c("NSW", "VIC", "QLD", "SA", "WA", "NT", "ACT", "TAS"), 2)
#' gc <- rbind(sex, state)  # a matrix consists of strings.
#' gn <- rbind(rep(1:2, each = 8), rep(1:8, 2))  # a numerical matrix
#' rownames(gc) <- rownames(gn) <- c("Sex", "State")
#' x <- gts(abc, groups = gc)
#' y <- gts(abc, groups = gn)
#' 
#' # Example 2 with two simple hierarchies (geography and product) to show the argument "characters"
#' bnames1 <- c("VICMelbAA", "VICMelbAB", "VICGeelAA", "VICGeelAB",  
#'              "VICMelbBA", "VICMelbBB", "VICGeelBA", "VICGeelBB",
#'              "NSWSyndAA", "NSWSyndAB", "NSWWollAA", "NSWWollAB", 
#'              "NSWSyndBA", "NSWSyndBB", "NSWWollBA", "NSWWollBB")
#' bts1 <- matrix(ts(rnorm(160)), ncol = 16)
#' colnames(bts1) <- bnames1
#' x1 <- gts(bts1, characters = list(c(3, 4), c(1, 1)))
#' 
#' # Example 3 with a non-hierarchical grouped time series of 3 grouping variables (state, age and sex)
#' bnames2 <- c("VIC1F", "VIC1M", "VIC2F", "VIC2M", "VIC3F", "VIC3M",
#'              "NSW1F", "NSW1M", "NSW2F", "NSW2M", "NSW3F", "NSW3M")
#' bts2 <- matrix(ts(rnorm(120)), ncol = 12)
#' colnames(bts2) <- bnames2
#' x2 <- gts(bts2, characters = c(3, 1, 1))
#' 
#' @export
gts <- function(y, groups, gnames = rownames(groups), characters) {
  # Construct the grouped time series.
  #
  # Args:
  #   y*: The bottom time series assigned by the user.
  #   groups: A matrix contains the distinctive No. for each group at each row.
  #   gnames: Specify the group names.
  #   characters: Specify how to split the bottom names in order to generate
  #     the grouping matrix
  #
  # Returns:
  #   A grouped time series.
  #
  # Error handling:
  if (!is.ts(y)) {
    y <- stats::as.ts(y)
  }

  if (ncol(y) <= 1L) {
    stop("Argument y must be a multivariate time series.", call. = FALSE)
  }
  bnames <- colnames(y)
  nc.y <- ncol(y)
  if (missing(characters)) {
    if (missing(groups)) {
      groups <- matrix(c(rep(1L, nc.y), seq(1L, nc.y)), nrow = 2L,
                     byrow = TRUE)
    } else if (!is.matrix(groups)) {
      stop("Argument groups must be a matrix.", call. = FALSE)
    } else if (!is.character(groups[1L, ])) { # Check groups numeric matrix
      if (all(groups[1L, ] == 1L)) { # if the first row is all 1's
        groups <- groups[-1L, , drop = FALSE]
      }
      tmp.last <- nrow(groups)
      if (all(groups[tmp.last, ] == seq(1L, nc.y))) { # if the last row is a seq
        groups <- groups[-tmp.last, , drop = FALSE]
      }
    }
    # Check whether groups is unique
    # But R takes so long to check due to the inefficiency with strings
    # bgroup <- unique(apply(groups, 2, paste, collapse = ""))
    # if (ncol(groups) != ncol(y) && length(bgroup) != ncol(y)) {
    #   stop("Argument groups is misspecified.")
    # }
  } else {
    if (length(characters) == 1L) {
      stop("The argument characters must have length greater than one.", call. = FALSE)
    }
    if (!all(nchar(bnames)[1L] == nchar(bnames)[-1L])) {
      stop("The bottom names must be of the same length.", call. = FALSE)
    }
    if (any(nchar(bnames) != sum(unlist(characters)))) {
      warning("The argument characters is not fully specified for the bottom names.")
    }
    groups <- CreateGmat(bnames, characters)
  }
  # Construct gmatrix
  gmat <- GmatrixG(groups)  # GmatrixG() defined below

  # Construct gnames
  nr.gmat <- nrow(gmat)
  if (nr.gmat == 2L) {
    name.list <- NULL
  } else if (is.null(gnames)) {
    message("Argument gnames is missing and the default labels are used.")
    gnames <- paste0("G", 1L:(nr.gmat - 2L))
  }
  colnames(gmat) <- bnames
  rownames(gmat) <- c("Total", gnames, "Bottom")

  # Keep the names at each group
  if (nr.gmat > 2L) {
    times <- Mlevel(groups)
    full.groups <- mapply(rep, as.list(gnames), times, SIMPLIFY = FALSE)
    subnames <- apply(groups, 1, unique)
    if (is.matrix(subnames)) {
      # Convert a matrix to a list
      subnames <- split(subnames, rep(1L:ncol(subnames), each = nrow(subnames)))
    }
    name.list <- mapply(paste0, full.groups, "/", subnames, SIMPLIFY = FALSE)
    names(name.list) <- gnames
  }

  return(structure(
    list(bts = y, groups = gmat, labels = name.list),
    class = c("gts")
  ))
}


#' @rdname helper-functions
#' @export
get_groups <- function(y) {
  if(all(is.hts(y) && is.gts(y))) stop("'y' must be grouped time series.", call. = FALSE)
  return(y$groups)
}


# A function to convert groups to gmatrix
GmatrixG <- function(xmat) {
  if (is.character(xmat)) {
    # Convert character to integer
    gmat <- t(apply(xmat, 1, function(x) as.integer(factor(x, unique(x)))))
  } else {
    gmat  <- xmat
  }
  # Insert the first & last rows
  nc.xmat <- ncol(xmat)
  gmat <- rbind(rep(1L, nc.xmat), gmat, seq(1L, nc.xmat))
  gmat <- gmat[!duplicated(gmat), , drop = FALSE] # Remove possible duplicated
  return(structure(gmat, class = "gmatrix"))
}


# A function to calculate No. of groups at each level
Mlevel <- function(xgroup) {
  m <- apply(xgroup, 1, function(x) length(unique(x)))
  return(m)
}


# A function to get the inverse of row sums of S matrix
InvS4g <- function(xgroup) {
  mlevel <- Mlevel(xgroup)
  len <- length(mlevel)
  repcount <- mlevel[len]/mlevel
  inv.s <- 1/unlist(mapply(rep, repcount, mlevel, SIMPLIFY = FALSE))
  return(inv.s)
}


# A function to generate the gmatrix based on bottom names
CreateGmat <- function(bnames, characters) {
  total.len <- length(characters)
  sub.len <- c(0L, lapply(characters, length))
  cs <- cumsum(unlist(sub.len))
  int.char <- unlist(characters)
  end <- cumsum(int.char)
  start <- end - int.char + 1L
  tmp.token <- sapply(bnames, function(x) substring(x, start, end))
  # Grab the individual group
  token <- vector(length = total.len, mode = "list")
  for (i in 1L:total.len) {
    token[[i]] <- matrix(, nrow = sub.len[[i + 1L]], ncol = ncol(tmp.token))
  }
  for (i in 1L:total.len) {
    token[[i]][1L, ] <- tmp.token[cs[i] + 1L, ]
    if (sub.len[[i + 1L]] >= 2L) {
      for (j in 2L:sub.len[[i + 1L]]) {
        token[[i]][j, ] <- paste0(token[[i]][j - 1L, ], tmp.token[cs[i] + j, ])
      }
    }
  }
  # Take combinations of any two groups
  cn <- combn(1L:total.len, 2)
  ncl <- ncol(cn)
  groups <- vector(length = ncl, mode = "list")
  for (i in 1L:ncl) {
    bigroups <- list(token[[cn[, i][1L]]], token[[cn[, i][2L]]])
    nr1 <- nrow(bigroups[[1L]])
    nr2 <- nrow(bigroups[[2L]])
    nr <- nr1 * nr2
    tmp.groups <- vector(length = nr1, mode = "list")
    for (j in 1L:nr1) {
      tmp.groups[[j]] <- paste0(bigroups[[1L]][j, ], bigroups[[2L]][1L, ])
      if (nr2 >= 2L) {
        for (k in 2L:nr2) {
          tmp.groups[[j]] <- rbind(tmp.groups[[j]], paste0(bigroups[[1L]][j, ],
                                   bigroups[[2L]][k, ]))
        }
      }
    }
    groups[[i]] <- tmp.groups[[1L]]
    if (nr1 >= 2L) {
      for (h in 2L:nr1) {
        groups[[i]] <- rbind(groups[[i]], tmp.groups[[h]])
      }
    }
  }
  # Combine the individual ones and their combinations
  new.list <- c(token, groups)
  gmatrix <- new.list[[1L]]
  for (i in 2L:length(new.list)) {
    gmatrix <- rbind(gmatrix, new.list[[i]])
  }
  gmatrix <- gmatrix[!duplicated(gmatrix), , drop = FALSE]
  # Remove bottom names if it has
  check <- try(which(gmatrix == bnames, arr.ind = TRUE)[1L, 1L], silent = TRUE)
  if (class(check) != "try-error") {
    gmatrix <- gmatrix[-check, ]
  }
  return(gmatrix)
}

#' @rdname gts-class
#' @param xts \code{gts} object.
#' @export
# A function to check whether it's the "gts" class.
is.gts <- function(xts) {
  is.element("gts", class(xts))
}

#' @rdname gts-class
#' @param x \code{gts} object.
#' @method print gts
#' @export
#' @export print.gts
# Print "gts" on the screen
print.gts <- function(x, ...) {
  cat("Grouped Time Series \n")
  nlevels <- Mlevel(x$groups)
  cat(length(nlevels), "Levels \n")
  cat("Number of groups at each level:", nlevels, "\n")
  cat("Total number of series:", sum(nlevels), "\n")

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

#' @rdname gts-class
#' @param object \code{gts} object.
#' @method summary gts
#' @export
#' @export summary.gts
summary.gts <- function(object, ...) {
  print(object)
  if (is.null(object$histy)) {
    cat("\n")
    cat("Labels: \n")
    print(names(object$labels))
  } else {
    method <- switch(object$method,
    comb = "Optimal combination forecasts",
    bu = "Bottom-up forecasts",
    mo = "Middle-out forecasts",
    tdgsa = "Top-down forecasts based on the average historical proportions",
    tdgsf = "Top-down forecasts based on the proportion of historical averages",
    tdfp = "Top-down forecasts using forecasts proportions")
    fmethod <- switch(object$fmethod, ets = "ETS", arima = "Arima", 
                      rw = "Random walk")
    cat("\n")
    cat(paste("Method:", method), "\n")
    cat(paste("Forecast method:", fmethod), "\n")
    if (!is.null(object$fitted)) {
      cat("In-sample error measures at the bottom level: \n")
      print(accuracy.gts(object))
    }
  }
}

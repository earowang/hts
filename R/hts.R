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

  if (nbts <= 1L) {
    stop("Argument y must be a multivariate time series.")
  }
  if (missing(characters)) { # Arg "characters" not specified
    message("Since argument characters are not specified, the default labelling system is used.")
    if (missing(nodes)) {
      nodes <- list(nbts)
    }
    if (!is.list(nodes)) {
      stop("Argument nodes must be a list.")
    }
    if (length(nodes[[1L]]) != 1L) {
      stop("The root node cannot be empty.")
    }
    if (sum(nodes[[length(nodes)]]) != nbts) {
      stop("The number of terminal nodes is not consistent with the number of bottom time series.")
    }
    if (length(nodes) > 1L) {
      for (i in 1L:(length(nodes) - 1L)) {
        if (sum(nodes[[i]]) != length(nodes[[i + 1]])) {
          error <- sprintf("The number of nodes for the level %i is not equal to the number of series of level %i.", i - 1L, i)
          stop(error)
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
      stop("The bottom names must be of the same length.")
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

  output <- structure(list(bts = y, nodes = nodes, labels = labels),
                      class = c("gts", "hts"))
  return(output)
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

# A function to check whether it's the "hts" class.
is.hts <- function(xts) {
  is.element("hts", class(xts))
}

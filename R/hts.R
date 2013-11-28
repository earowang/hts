# Functions to contruct hierarchical or grouped time series.

hts <- function(y, nodes, bnames = colnames(y), characters) {
  # Construct the hierarchical time series.
  # 
  # Args:
  #   y*: The bottom time series assigned by the user. Same lengths and no NA.
  #   nodes: A list contains the number of child nodes for each level except
  #     for the bottom one. If missing, it's assumed to have only one level.
  #   bnames: The names of the bottom time series.
  #   characters: Define how to split the "bnames" in order to construct the
  #     level labels. Otherwise, use the defaul labelling system.
  #
  # Returns:
  #   A hierarchical time series.
  #
  # ToDo:
  #   1. May handle NA's by forecasting them properly.
  #
  # Error handling:
  if(!is.ts(y)) {
    stop("Agrument y must be a time series data.")
  }
  if(ncol(y) <= 1L) {
    stop("Argument y must be a multiviate time series.")
  }
  if(any(is.na(y))) {
    stop("Argument y must not have missing values.")
  }
  if(missing(nodes)) {
    nodes <- list(ncol(y))
  } 
  if(!is.list(nodes)) {
    stop("Argument nodes must be a list.")
  } 
  if(length(nodes[[1L]]) != 1L) {
    stop("The root node cannot be empty.")
  }
  if(sum(nodes[[length(nodes)]]) != ncol(y)) {
    stop("The number of terminal nodes is not consistent with the number of 
         bottom time series.")
  }
  if(length(nodes) > 1L) {
    for(i in 1:(length(nodes) - 1)) {
      if(sum(nodes[[i]]) != length(nodes[[i + 1]])) {
        error <- paste("The number of nodes for the level", i - 1, "is not 
                       equal to the number of series of level", i)
        stop(error)
      }
    }
  }

  # Construct the level labels
  if(is.null(bnames) || missing(characters)) {
    message("Since argument bnames/characters are not specified, the default 
            labelling system is used.")
    labels <- HierName(nodes) # HierName() defined below
  }

  # Obtain other information
  names(nodes) <- paste("Level", 0:(length(nodes) - 1))

  output <- structure(list(bts = y, nodes = nodes), class = "hts")
  return(output)
}


# A function to convert the nodes list to gmatrix
Gmatrix <- function(xlist) {
  num.bts <- sum(xlist[[length(xlist)]])
  # Create an empty matrix to contain the gmatrix
  gmat <- matrix(, nrow = length(xlist) + 1, ncol = num.bts)
  # Insert the bottom level
  gmat[nrow(gmat), ] <- seq(1, num.bts)
  # Insert the middle levels in the reverse order
  for(i in length(xlist):2) {
    gint <- integer(length = num.bts)
    for(k in 1:length(xlist[[i]])) {
      # Returns the NO. of the unique numbers for each block of the lower level
      num.unique <- cumsum(xlist[[i]])
      if(k == 1L) {
        index <- seq(1, num.unique[k])
      } else {
        index <- seq(1, num.unique[k])[-seq(1, num.unique[k - 1])]
      }
      block <- unique(gmat[i + 1, ])[index]
      # Returns the full index for each block
      block.index  <- which(gmat[i + 1, ] %in% block)
      block.length <- length(gmat[i + 1, block.index])
      gint[block.index] <- rep(k, block.length)
    }
    gmat[i, ] <- gint
  }
  # Insert the top level
  gmat[1, ] <- rep(1L, num.bts)

  colnames(gmat) <- colnames(xlist)
  rownames(gmat) <- paste("Level", 0:(nrow(gmat) - 1))
  class(gmat) <- "gmatrix"
  return(gmat)
}


# A function to return the NO. of nodes at each level
Mnodes <- function(xlist) {
  m <- c(unlist(lapply(xlist, length)), sum(xlist[[length(xlist)]]))
  return(m)
}


# A function to set the default hierarchical names
HierName<- function(xlist) {
  gmat <- Gmatrix(xlist)  # Based on gmatrix
  names.mat <- gmat[-1L, ]
  for(i in nrow(names.mat):1) {
    no.unique <- cumsum(xlist[[i]])
    for(k in 1:length(no.unique)) {
      if(k == 1L) {
        index <- which(names.mat[i, ] %in% seq(1, no.unique[k]))
      } else {
        index <- which(names.mat[i, ] %in% seq(no.unique[k - 1], no.unique[k]))
      }
      letter <- length(unique(names.mat[i, index]))
      times <- as.data.frame(table(names.mat[i, index]))
      names.mat[i, index] <- rep(LETTERS[1:letter], times[, 2])
    }
  }
  j <- 2
  while(j <= nrow(names.mat)) {
    # Overwrite names.mat
    names.mat[j, ] <- paste0(names.mat[j - 1, ], names.mat[j, ])
    j <- j + 1
  }
  names.list <- c("Level 0" = "Total", apply(names.mat, 1, unique))
  return(names.list)
}


# A function to check whether it's the "hts" class.
is.hts <- function(xts) {
  is.element("hts", class(xts))
}


# Print "hts" on the screen
print.hts <- function(xts) {
  # ToDo:
  #   1. Add if condition (fcasts) exists
  bts <- xts$bts
  cat("Hierarchical Time Series \n")
  cat(length(Mnodes(xts$nodes)), "Levels \n")
  cat("Number of nodes at each level:", Mnodes(xts$nodes), "\n")
  cat("Total number of series:", sum(Mnodes(xts$nodes)), "\n")
  cat("Number of observations per series:", nrow(bts), "\n")
  cat("Top level series:", "\n")
  
  topts <- ts(rowSums(bts), start = tsp(bts)[1L], frequency = tsp(bts)[3L])
  print(topts)
}

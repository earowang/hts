# A function to implement Bottom-up approach
BottomUp <- function(bfcasts, gmatrix) {
  levels <- 1L:nrow(gmatrix)
  allfcasts <- lapply(levels, function(x) rowsum(t(bfcasts), gmatrix[x, ]))
  # Convert lists to matrices
  allfcasts <- matrix(unlist(sapply(allfcasts, t)), nrow = nrow(bfcasts))
  return(allfcasts)
}

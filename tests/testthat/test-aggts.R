# A unit test for aggts() function
test_that("tests for a non-gts object (input)", {
  set.seed(1234)
  mts <- ts(matrix(5 + sort(rnorm(500)), nrow = 50, ncol = 10))
  
  expect_error(aggts(mts))
})

test_that("tests for a non-gts object (output)", {
  set.seed(1234)
  mts <- ts(matrix(5 + sort(rnorm(500)), nrow = 50, ncol = 10))
  node.list <- list(3, c(2, 3, 1), c(2, 2, 1, 1, 1, 3))
  hts <- hts(mts, nodes = node.list)
  out <- dim(aggts(hts))
  
  expect_identical(out, c(50L, 20L))
})

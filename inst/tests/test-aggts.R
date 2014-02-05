# A unit test for aggts() function
context("Tests on input")
test_that("tests for a non-gts object", {
  set.seed(1234)
  mts <- ts(matrix(5 + sort(rnorm(500)), nrow = 50, ncol = 10))
  
  expect_that(aggts(mts), throws_error())
})

context("Tests on output")
test_that("tests for a non-gts object", {
  set.seed(1234)
  mts <- ts(matrix(5 + sort(rnorm(500)), nrow = 50, ncol = 10))
  node.list <- list(3, c(2, 3, 1), c(2, 2, 1, 1, 1, 3))
  hts <- hts(mts, nodes = node.list)
  out <- dim(aggts(hts))
  
  expect_that(out, equals(c(50, 20)))
})

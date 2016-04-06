# A unit test for hts() function
context("Tests on inputs")

test_that("tests for y as a mts", {
  set.seed(1234)
  sts <- ts(rnorm(100), start = c(2001, 1), frequency = 12)
  node.list <- list(3, c(2, 3, 1), c(2, 2, 1, 1, 1, 3))

  expect_that(hts(sts, node.list), throws_error())
})

test_that("tests for node as a list", {
  set.seed(1234)
  mts <- ts(matrix(5 + sort(rnorm(500)), nrow = 50, ncol = 10))
  node.mat <- matrix(1:10, nrow = 2, ncol = 5)

  expect_that(hts(mts, node.mat), throws_error())
})

test_that("tests for node by default", {
  set.seed(1234)
  mts <- ts(matrix(5 + sort(rnorm(500)), nrow = 50, ncol = 10))
  nodes <- list("Level 1" = 10)

  expect_that(hts(mts)$nodes, equals(nodes))
})

test_that("tests for the root node not specified", {
  set.seed(1234)
  mts <- ts(matrix(5 + sort(rnorm(500)), nrow = 50, ncol = 10))
  node.list <- list(c(2, 3, 1), c(2, 2, 1, 1, 1, 3))

  expect_that(hts(mts, node.list), throws_error())
})

test_that("tests for the terminal nodes wrong", {
  set.seed(1234)
  mts <- ts(matrix(5 + sort(rnorm(500)), nrow = 50, ncol = 10))
  node.list <- list(1, c(2, 3, 1), c(2, 2, 1, 2, 1, 3))

  expect_that(hts(mts, node.list), throws_error())
})

test_that("tests for the middle nodes wrong", {
  set.seed(1234)
  mts <- ts(matrix(5 + sort(rnorm(500)), nrow = 50, ncol = 10))
  node.list <- list(1, c(2, 4, 1), c(2, 2, 1, 1, 1, 3))

  expect_that(hts(mts, node.list), throws_error())
})

context("tests on output")

test_that("tests for the gmatrix", {
  node.list <- list(3, c(2, 3, 1), c(2, 2, 1, 1, 1, 3))
  g <- matrix(c(rep(1, 10), rep(1, 4), rep(2, 3), rep(3, 3), rep(1, 2), 
                rep(2, 2), seq(3, 5), rep(6, 3), seq(1, 10)), ncol = 10, 
                byrow = TRUE)
  class(g) <- "gmatrix"

  output <- GmatrixH(node.list)
  dimnames(output) <- NULL
  expect_that(output, equals(g))
})

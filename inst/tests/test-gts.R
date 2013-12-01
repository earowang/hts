# A unit test for gts() function
context("Tests on output")

test_that("tests for groups by default", {
  set.seed(1234)
  mts <- ts(5 + matrix(sort(rnorm(2700)), nrow = 100, ncol = 27), 
            start = c(2001, 1), frequency = 12)

  gts(mts)$groups
  expect_that(gts(mts)$gnames, equals(c("Total", "Bottom")))
})

test_that("tests for gnames", {
  set.seed(1234)
  mts <- ts(5 + matrix(sort(rnorm(2700)), nrow = 100, ncol = 27), 
            start = c(2001, 1), frequency = 12)
  g <- matrix(c(rep(1:3, each = 9), rep(c(rep(1, 3), rep(2, 3), rep(3, 3)), 3),
              rep(1:3, 9)), nrow = 3, byrow = 9)
  output <- c("Total", paste("Group", LETTERS[1:3]), "Bottom")
  expect_that(gts(mts, g)$gnames, equals(output))
})

test_that("tests for gmatrix", {
  set.seed(1234)
  mts <- ts(5 + matrix(sort(rnorm(2700)), nrow = 100, ncol = 27), 
            start = c(2001, 1), frequency = 12)
  g <- matrix(c(rep(1:3, each = 9), rep(c(rep(1, 3), rep(2, 3), rep(3, 3)), 3),
              rep(1:3, 9)), nrow = 3, byrow = 9)
  gmat <- rbind(rep(1, 27), g, seq(1, 27))
  class(gmat) <- "gmatrix"
  output <- gts(mts, g)$groups
  dimnames(output) <- NULL

  expect_that(output, equals(gmat))
})

test_that("tests for matrix with characters", {
  set.seed(1234)
  mts <- ts(5 + matrix(sort(rnorm(1600)), nrow = 100, ncol = 16), 
            start = c(2001, 1), frequency = 12)
  gchar <- matrix(c(rep("Male", 8), rep("Female", 8), rep(LETTERS[3:10], 2)),
                  nrow = 2, byrow = T)
  g <- matrix(c(rep(1, 8), rep(2, 8), rep(1:8, 2)), nrow = 2, byrow = T)
  gmat <- rbind(rep(1, 16), g, seq(1, 16))
  class(gmat) <- "gmatrix"
  output <- gts(mts, gchar)$groups
  dimnames(output) <- NULL

  expect_that(output, equals(gmat))
})

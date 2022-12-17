# A unit test for gts() function
test_that("tests for labels", {
  set.seed(1234)
  mts <- ts(5 + matrix(sort(rnorm(2700)), nrow = 100, ncol = 27), 
            start = c(2001, 1), frequency = 12)
  g <- matrix(c(rep(1:3, each = 9), rep(c(rep(1, 3), rep(2, 3), rep(3, 3)), 3),
              rep(1:3, 9)), nrow = 3, byrow = TRUE)
  output <- paste0("G", 1:3)
  expect_named(gts(mts, g)$labels, output)
})

test_that("tests for specified labels", {
  set.seed(1234)
  mts <- ts(5 + matrix(sort(rnorm(2700)), nrow = 100, ncol = 27), 
            start = c(2001, 1), frequency = 12)
  g <- matrix(c(rep(1:3, each = 9), rep(c(rep(1, 3), rep(2, 3), rep(3, 3)), 3),
              rep(1:3, 9)), nrow = 3, byrow = TRUE)
  rownames(g) <- c("Sex", "Purpose", "Frames")
  expect_named(gts(mts, g)$labels, rownames(g))
})

test_that("tests for gmatrix", {
  set.seed(1234)
  mts <- ts(5 + matrix(sort(rnorm(2700)), nrow = 100, ncol = 27), 
            start = c(2001, 1), frequency = 12)
  g <- matrix(c(rep(1:3, each = 9), rep(c(rep(1, 3), rep(2, 3), rep(3, 3)), 3),
              rep(1:3, 9)), nrow = 3, byrow = TRUE)
  gmat <- rbind(rep(1, 27), g, seq(1, 27))
  class(gmat) <- "gmatrix"
  output <- gts(mts, g)$groups
  dimnames(output) <- NULL

  expect_identical(output, gmat)
})

test_that("tests for matrix with characters", {
  set.seed(1234)
  mts <- ts(5 + matrix(sort(rnorm(1600)), nrow = 100, ncol = 16), 
            start = c(2001, 1), frequency = 12)
  gchar <- matrix(c(rep("Male", 8), rep("Female", 8), rep(LETTERS[3:10], 2)),
                  nrow = 2, byrow = TRUE)
  g <- matrix(c(rep(1L, 8L), rep(2L, 8L), rep(1:8, 2)), nrow = 2, byrow = TRUE)
  gmat <- rbind(rep(1L, 16L), g, 1:16)
  class(gmat) <- "gmatrix"
  output <- gts(mts, gchar)$groups
  dimnames(output) <- NULL

  expect_identical(output, gmat)
})

# A unit test for the inverse of row sums of smatrix
test_that("tests for hts", {
  set.seed(1234)
  mts <- ts(matrix(5 + sort(rnorm(500)), nrow = 50, ncol = 10))
  node.list <- list(3, c(2, 3, 1), c(2, 2, 1, 1, 1, 3))
  hts <- hts(mts, nodes = node.list)
  s <- 1/rowSums(smatrix(hts))

  expect_that(InvS4h(node.list), equals(s))
})

test_that("tests for gts", {
  set.seed(1234)
  mts <- ts(5 + matrix(sort(rnorm(2700)), nrow = 100, ncol = 27), 
            start = c(2001, 1), frequency = 12)
  g <- matrix(c(rep(1:3, each = 9), rep(c(rep(1, 3), rep(2, 3), rep(3, 3)), 3),
              rep(1:3, 9)), nrow = 3, byrow = TRUE)
  gts <- gts(mts, groups = g)
  s <- 1/rowSums(smatrix(gts))
  out <- InvS4g(gts$groups)
  names(out) <- NULL

  expect_that(out, equals(s))
})

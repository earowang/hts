# A unit test for combinef() function
test_that("tests for hts at the bottom level (inputs)", {
  set.seed(1234)
  mts <- ts(matrix(5 + sort(rnorm(500)), nrow = 50, ncol = 10))
  node.list <- list(3, c(2, 3, 1), c(2, 2, 1, 1, 1, 3))

  expect_error(combinef(mts, nodes = node.list, algorithms = "lu"))
})

test_that("tests for hts (outputs)", {
  set.seed(1234)
  mts <- ts(matrix(5 + sort(rnorm(50)), nrow = 5, ncol = 10))
  node.list <- list(3, c(2, 3, 1), c(2, 2, 1, 1, 1, 3))
  hts <- hts(mts, nodes = node.list)
  allf <- allts(hts)
  out1 <- combinef(allf, nodes = node.list, keep = "bottom", algorithms = "lu")
  out2 <- combinef(allf, nodes = node.list, keep = "gts", algorithms = "lu")

  expect_identical(dim(out1), c(5L, 10L))
  expect_true(is.hts(out2))
})

test_that("tests for gts (outputs)", {
  set.seed(1234)
  mts <- ts(5 + matrix(sort(rnorm(270)), nrow = 10, ncol = 27), 
            start = c(2001, 1), frequency = 12)
  g <- matrix(c(rep(1:3, each = 9), rep(c(rep(1, 3), rep(2, 3), rep(3, 3)), 3),
              rep(1:3, 9)), nrow = 3, byrow = TRUE)
  gts <- gts(mts, groups = g)
  out1 <- combinef(allts(gts), groups = gts$groups, keep = "bottom",
                   algorithms = "lu")
  out2 <- combinef(allts(gts), groups = g, keep = "gts", algorithms = "lu")

  expect_identical(dim(out1), c(10L, 27L))
  expect_identical(dim(out2$bts), c(10L, 27L))
})

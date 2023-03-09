test_that("tcov() works correctly", {
  X <- iris[, 1:4]
  expect_equal(tcov(X), ICSClust:::tcov_amap(X))
})

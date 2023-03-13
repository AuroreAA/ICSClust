test_that("tcov() works correctly", {
  X <- iris[, 1:4]
  expect_equal(tcov(X), ICSClust:::tcov_amap(X))
})

# test_that("scov() works correctly", {
#   X <- iris[, 1:4]
#   expect_equal(scov(X, beta = 4), ICSClust:::scov_amap(X, beta = 4))
# })
test_that("ucov() works correctly", {
  X <- iris[, 1:4]
  beta = 4
  expect_equal(ucov(X, beta = beta), ICSClust:::ucov_amap(X, beta = beta))
})







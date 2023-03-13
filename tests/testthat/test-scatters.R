test_that("tcov() works correctly", {
  X <- iris[, 1:4]
  expect_equal(tcov(X), ICSClust:::tcov_amap(X))
})

# test_that("scov() works correctly", {
#   X <- iris[, 1:4]
#   expect_equal(scov(X), ICSClust:::scov_amap(X))
# })

test_that("ucov() works correctly", {
  X <- iris[, 1:4]
  expect_equal(ucov(X), ICSClust:::ucov_amap(X))
})







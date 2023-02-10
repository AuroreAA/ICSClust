test_that("rcpp function works", {
  expect_equal(rcpp_hello_world(), list(c("foo","bar"), 0:1))
})

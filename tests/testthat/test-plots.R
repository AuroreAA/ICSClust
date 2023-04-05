# # plotest_that("multiplication works", {
# #   expect_equal(2 * 2, 4)
# # })
# 
# 
# X <- iris[,-5]
# res <- ICS(X)
# screeplot_crit.ICS(res)
# pairs_plot(res)
# pairs_plot(res, select = c(1,4))
# pairs_plot(res, clusters = iris[,5])
# pairs_plot(res, select = c(1,4), clusters = iris[,5])
# pairs_plot(X, select = c(1,4), clusters = iris[,5])

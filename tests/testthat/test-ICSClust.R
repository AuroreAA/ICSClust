# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
# 
# 
# X <- iris[,1:4]
# res <- ICS(X, S1 = ICS_cov, S2 = ICS_cov4)
# 
# 
# 
# ICS_comp_crit(res, crit = "normal", comp_max = NULL, crit_args = list(level = 0.1, test = "agostino.test"))
# 
# ICS_comp_crit(res, crit = "med", comp_max = 2)
# 
# ICS_comp_crit(res, crit = "var", comp_max = 1)
# 
# ICS_comp_crit(res, crit = "discriminatory", groups_vec = iris[,5], comp_max = 2)
# 
# ICSClust(res, crit = "med", comp_max = 2,
#          clustering_fun = "kmeansClust",
#          clustering_args = list(), nb_clusters = 3)
# 
# res_clust <- ICSClust(res, crit = "med", comp_max = 2,
#          clustering_fun = "tkmeansClust",
#          clustering_args = list(alpha = 0.05), nb_clusters = 3,
#          groups_vec = iris[,5])
# res_clust
# 
# res_clust <- ICSClust(res, crit = "med", comp_max = 1,
#                       clustering_fun = "kmeansClust", nb_clusters = 3,
#                       groups_vec = iris[,5])
# res_clust
# 
# 

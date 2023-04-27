# X <- iris[,-5]
# ICSClust(X)
# 
# res <- ICSClust(X, nb_clusters = 3)
# res <- ICSClust(X, nb_select = 2, nb_clusters = 3)
# res
# table(res$clusters)
# table(res$clusters, iris[,5])
# component_plot(res$ICS_out, select = res$select, clusters = as.factor(res$clusters))
# component_plot(res$ICS_out, select = res$select, clusters = iris[,5])
# 
# 
# res <- ICSClust(X, nb_select = 1, nb_clusters = 3,
#                 ICS_args = list(S1 = ICS_mcd, S2 = ICS_cov,
#                                 S1_args = list(alpha = 0.5)))
# table(res$clusters, iris[,5])
# component_plot(res$ICS_out, clusters = as.factor(res$clusters))
# component_plot(res$ICS_out, clusters = iris[,5])
#
#
# res <- ICSClust(X, nb_clusters = 3,
#                 ICS_args = list(S1 = ICS_mcd, S2 = ICS_cov,
#                                 S1_args = list(alpha = 0.5)),
#                 criterion = "normal_crit",
#                 ICS_crit_args = list(level = 0.1, test = "anscombe.test",
#                                      max_select = NULL))
# table(res$clusters, iris[,5])
# component_plot(res$ICS_out, select = res$select, clusters = as.factor(res$clusters))
# component_plot(res$ICS_out, select = res$select, clusters = iris[,5])
#
# res <- ICSClust(X, nb_select = 1, nb_clusters = 3,
#                 ICS_args = list(S1 = ICS_mcd, S2 = ICS_cov,
#                                 S1_args = list(alpha = 0.5)),
#                 method  = "tkmeans_clust",
#                 clustering_args = list(alpha = 0.1))
# table(res$clusters, iris[,5])
# component_plot(res$ICS_out, clusters = as.factor(res$clusters))
# component_plot(res$ICS_out, clusters = iris[,5])
#

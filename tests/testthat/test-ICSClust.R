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
# 
# 
# pct_clusters = c(0.5,0.5)
# n = 1000
# p = 100
# delta = 10
# epsilons <- c(0, 0.02, 0.05)
# epsilon_max <- max(epsilons)
# epsilon <- 0.05
# mult <- 2
# 
# data <- mixture_sim(pct_clusters = pct_clusters, n = n, p = p,
#                     delta = delta)
# nb_clusters <- length(unique(data$cluster))
# # define nb_select by default equals to the number of clusters -1
# nb_select <- length(pct_clusters)-1
# 
# 
# component_plot(data[,-1], clusters = as.factor(data$cluster))
# 
# 
# # outliers
# # generate probabilities of being an outlier
# outlier_probabilities <- runif(n)
# order <- order(outlier_probabilities)
# data <- data[order, ]
# outlier_probabilities <- outlier_probabilities[order]
# 
# # generate outlying values to be used
# n_outliers_max <- sum(outlier_probabilities < epsilon_max)
# min_all <- apply(data[,-1], 2, min)
# max_all <- apply(data[,-1], 2, max)
# data_outliers <- runif_outside_range(n = n_outliers_max, min = min_all,
#                                      max = max_all, mult = mult)
# data_outliers <- data.frame(clusters = "outliers", data_outliers)
# 
# outliers <- which(outlier_probabilities < epsilon)
# data[outliers, ] <- data_outliers[outliers, ]
# 
# component_plot(data[,-1], clusters = as.factor(data$cluster))
# 
# # clustering
# library(mclust)
# mod1 <- Mclust(data[,-1])
# summary(mod1)
# table(data$cluster, mod1$classification)
# 
# 
# system.time(fmod1 <- Mclust(data[,-1], initialization = list(noise = TRUE)))
# summary(mod1)
# table(data$cluster, mod1$classification)
# 
# system.time(mod2 <- Mclust(data[,-1], modelNames = "EII", G = 2,
#                initialization = list(noise = TRUE)))
# summary(mod2)
# table(data$cluster, mod2$classification)
# component_plot(data[,-1], clusters = as.factor(data$cluster))
# 
# 
# # rimle
# system.time(clust <- otrimle::rimle(data = data, G = 2, npr.max = 0.05))
# #summary(clust)
# table(data$cluster, clust$cluster)
# 
# # otrimle
# system.time(clust <- otrimle::otrimle(data = data, G = 2, npr.max = 0.05))
# #summary(clust)
# table(data$cluster, clust$cluster)
# 




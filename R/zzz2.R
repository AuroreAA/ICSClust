# 
# # Data -----
# X <- iris[,1:4]
# 
# # Simulation ----
# 
# # Dimension reduction ----
# ## ICS ----
# ### Scatters ----
# res <- ICS::ICS(X, S1 = ICS_cov, S2 = ICS_cov4)
# 
# ### Selection of components ----
# 
# #### Normal -----
# normal_crit(res$scores, level = 0.05,  test = "agostino.test", index_max = NULL, index_only = FALSE)
# normal_crit(res, level = 0.05,  test = "agostino.test", index_max = NULL, index_only = FALSE)
# normal_crit(res, level = 0.05,  test = "agostino.test", index_max = NULL, index_only = TRUE)
# normal_crit(res$scores, level = 0.15,  test = "agostino.test", index_max = NULL, index_only = TRUE)
# 
# normal_crit(res, level = 0.05,  test = "jarque.test", index_max = NULL, index_only = TRUE)
# 
# normal_crit(res, level = 0.05,  test = "jarque.test", index_max = NULL, index_only = FALSE)
# 
# normal_crit(res, level = 0.05,  test = "bonett.test", index_max = NULL, index_only = TRUE)
# 
# normal_crit(res, level = 0.05,  test = "shapiro.test", index_max = NULL, index_only = TRUE)
# 
# 
# #### Med -----
# med_crit(res, index_max = 2, index_only = TRUE)
# med_crit(res$lambda, index_max = 2, index_only = FALSE)
# 
# #### Var -----
# var_crit(res, index_max = 2, index_only = TRUE)
# var_crit(res$lambda, index_max = 1, index_only = FALSE)
# 
# 
# #### Eta2 ----
# discriminatory_crit(res, clusters = iris[,5], index_max = 2, index_only = TRUE)
# discriminatory_crit(res$scores, clusters = iris[,5], index_max = 3, index_only = FALSE)
# 
# 
# 
# index <- med_crit(lambda(res), index_max = 2, index_only = TRUE)
# 
# # Clustering ----
# # Parameters
# scaling = FALSE
# true_clusters = iris[,5]
# nb_clusters = length(unique(true_clusters))
# 
# ## Scaling ----
# # reduced_df <- components(res, index = index)
# reduced_df <- res$scores[, index]
# 
# if(scaling) reduced_df <- scale(reduced_df, center = TRUE, scale = TRUE)
# 
# ## Methods ----
# clusters <- do.call("kmeans_clust", list(df = reduced_df, k = nb_clusters, clusters_only = TRUE, iter.max = 100, nstart = 20))
# 
# 
# clusters <- do.call("tkmeans_clust", list(df = reduced_df, k = nb_clusters, clusters_only = TRUE,  alpha = 0.05))
# 
# 
# clusters <- do.call("pam_clust", list(df = reduced_df, k = nb_clusters, clusters_only = TRUE))
# ## Evaluation ----
# 
# rand <- mclust::adjustedRandIndex(true_clusters, clusters)
# rand

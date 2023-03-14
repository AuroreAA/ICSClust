# # load packages, but functions should still be called with package::function()
# library("parallel")
# library("ICS")
# 
# 
# # control parameters for data generation
# n <- 1000                               # number of observations
# p <- 10                                 # number of variables
# delta <- 10                             # shift location
# R <- 10                                 # number of simulation runs
# seed <- 20230313                        # seed of the random number generator
# 
# # control parameters for outliers
# epsilons <- c(0, 0.02, 0.05)
# epsilon_max <- max(epsilons)
# mult <- 2
# 
# # control parameters for mixture weights
# # for dimension reduction
# pct_clusters_list <- list(c(0.50, 0.50), c(0.55, 0.45), c(0.60, 0.40),
#                           c(0.65, 0.35), c(0.70, 0.30), c(0.75, 0.35),
#                           c(0.80, 0.20), c(0.85, 0.15), c(0.90, 0.10),
#                           c(0.95, 0.05),
#                           c(1/3, 1/3, 1/3), c(0.30, 0.40, 0.30),
#                           c(0.20, 0.50, 0.30), c(0.10, 0.50, 0.40),
#                           c(0.10, 0.60, 0.30), c(0.10, 0.70, 0.20),
#                           c(0.10, 0.80, 0.10),
#                           c(0.20, 0.20, 0.20, 0.20, 0.20),
#                           c(0.10, 0.20, 0.20, 0.20, 0.30),
#                           c(0.10, 0.10, 0.20, 0.20, 0.40),
#                           c(0.10, 0.10, 0.10, 0.30, 0.40),
#                           c(0.10, 0.10, 0.20, 0.30, 0.30)
# 
# )
# # for clustering
# pct_clusters_list <- list(c(0.50, 0.50), c(0.70, 0.30), c(0.80, 0.20),
#                           c(0.90, 0.10),
#                           c(1/3, 1/3, 1/3), c(0.20, 0.50, 0.30),
#                           c(0.10, 0.80, 0.10),
#                           c(0.20, 0.20, 0.20, 0.20, 0.20),
#                           c(0.10, 0.10, 0.20, 0.20, 0.40)
# 
# )
# 
# pct_clusters_list <- list(c(0.50, 0.50), c(0.70, 0.30)
# 
# )
# 
# 
# # control parameters for ICS scatters
# 
# 
# # control parameters for ICS criteria
# criteria <- c("normal_crit", "med_crit", "var_crit", "discriminatory_crit")
# criteria <- c( "med_crit", "var_crit", "discriminatory_crit")
# criteria_args <- list(
#   normal_crit = list(level = 0.05,  test = "agostino.test"),
#   med_crit = list(),
#   var_crit = list(),
#   discriminatory_crit = list()
# )
# 
# 
# # it is very easy to use parallel computing on Unix systems, but not on Windows
# if (.Platform$OS.type == "windows") {
#   n_cores <- 1              # use only one CPU core
# } else {
#   n_cores <- 2              # number of CPU cores to be used
#   RNGkind("L'Ecuyer-CMRG")  # use parallel random number streams
# }
# 
# 
# # run simulation
# cat(paste(Sys.time(), ": starting ...\n"))
# set.seed(seed)
# results_list <- parallel::mclapply(seq_len(R), function(r) {
# 
#   # print simulation run
#   cat(paste(Sys.time(), sprintf(":   run = %d\n", r)))
# 
# 
#   # Simulation --------------------------------------------------------------
#   # loop over different mixture weights
#   results_clusters <- lapply(pct_clusters_list, function(pct_clusters) {
#     ## Initial data -----
#     # We simulate normal gaussian for each cluster with the first variable being true clusters
# 
#     data <- mixture_sim(pct_clusters = pct_clusters, n = n, p = p, delta = delta)
#     nb_clusters <- length(unique(data$cluster))
# 
# 
#     # generate probabilities of being an outlier
#     outlier_probabilities <- runif(n)
# 
#     # order observations according to probabilities of being outliers,
#     # which makes it easier to keep previous outliers the same as the
#     # contamination level increases (for maximum comparability)
#     order <- order(outlier_probabilities)
#     data <- data[order, ]
#     outlier_probabilities <- outlier_probabilities[order]
# 
#     # generate outlying values to be used
#     n_outliers_max <- sum(outlier_probabilities < epsilon_max)
#     min_all <- apply(data[,-1], 2, min)
#     max_all <- apply(data[,-1], 2, max)
#     data_outliers <- runif_outside_range(n = n_outliers_max, min = min_all,
#                                          max = max_all, mult = mult)
#     data_outliers <- data.frame(clusters = "outliers", data_outliers)
# 
#     ## Outliers ----
#     # loop over contamination levels
#     results_epsilon <- lapply(epsilons, function(epsilon) {
#       # turn selected observations into outliers: since the
#       # observations are sorted according to the probability of being outliers,
#       # this keeps previous outliers the same as the contamination
#       # level increases
#       if (epsilon > 0) {
#         outliers <- which(outlier_probabilities < epsilon)
#         data[outliers, ] <- data_outliers[outliers, ]
#       }
#       true_clusters <- data[,1]
#       info <- data.frame(Run = r, epsilon = epsilon, q = length(pct_clusters),
#                          clusters = paste(round(pct_clusters*100), collapse = "-"))
# 
#       # No Dimension reduction ----
#       reduced_df <- scale(data[,-1], center = TRUE, scale = TRUE)
# 
# 
#       # Compute discriminatory power
#       eta2 <- eta2_power(reduced_df, clusters = true_clusters,
#                          index = 1:ncol(reduced_df))
# 
# 
#       # kmeans
#       rand_kmeans <- tryCatch({
#         clusters_kmeans <- do.call("kmeans_clust", list(df = reduced_df, k = nb_clusters, clusters_only = TRUE, iter.max = 100, nstart = 20))
#         ARI <- mclust::adjustedRandIndex(true_clusters, clusters_kmeans)
#         cbind(info, criterion = NA_character_, scatter = "Observed~data",
#               method = "kmeans", ARI = ARI, eta2 = eta2, nb_select = p)
#       }, error = function(e) NULL, warning = function(w) NULL)
# 
# 
#       # tkmeans
#       rand_tkmeans <- tryCatch({
#         clusters_tkmeans <- do.call("tkmeans_clust", list(df = reduced_df, k = nb_clusters, clusters_only = TRUE,  alpha = 0.05))
#         ARI <- mclust::adjustedRandIndex(true_clusters, clusters_tkmeans)
#         cbind(info, criterion = NA_character_, scatter = "Observed~data",
#               method = "tkmeans", ARI = ARI, eta2 = eta2, nb_select = p)
#       }, error = function(e) NULL, warning = function(w) NULL)
# 
#       # pam
#       rand_pam <- tryCatch({
#         clusters_pam <- do.call("pam_clust", list(df = reduced_df, k = nb_clusters, clusters_only = TRUE))
#         ARI <- mclust::adjustedRandIndex(true_clusters, clusters_pam)
#         cbind(info, criterion = NA_character_, scatter = "Observed~data",
#               method = "PAM", ARI = ARI, eta2 = eta2, nb_select = p)
#       }, error = function(e) NULL, warning = function(w) NULL)
# 
#       # combine results
#       df_ARI_no_reduction <- rbind(rand_kmeans, rand_tkmeans, rand_pam)
# 
# 
# 
# 
# 
#       # ICS ----
#       res <- ICS::ICS(X = data[,-1], S1 = ICS_cov, S2 = ICS_cov4)
#       scatter = "ICS"
#       # define index_max by default equals to the number of clusters -1
#       index_max <- length(pct_clusters)-1
#       criteria_args[["discriminatory_crit"]] <-
#         append(criteria_args[["discriminatory_crit"]],
#                list(clusters = true_clusters))
# 
#       results_ARI_ICS <- lapply(criteria, function(criterion) {
#         # Update some arguments depending on criterion
#         if (criterion == "normal_crit") index_max <- NULL
#         # Select the components
#         index <- do.call(criterion,
#                          append(list(object = res, index_max = index_max,
#                                      index_only = TRUE),
#                                 criteria_args[[criterion]]))
#         nb_select <- length(index)
# 
# 
#         if(nb_select == 0) index <- 0
# 
#         # Compute discriminatory power
#         eta2 <-  tryCatch({eta2_power(ICS::components(res),
#                                       clusters =true_clusters, index = index)
#         },error = function(e) 0, warning = function(w) 0)
#         reduced_df <- ICS::components(res, index = index)
# 
#         # kmeans
#         ARI <- tryCatch({
#           clusters_kmeans <- do.call("kmeans_clust",
#                                      list(df = reduced_df, k = nb_clusters,
#                                           clusters_only = TRUE, iter.max = 100,
#                                           nstart = 20))
#           mclust::adjustedRandIndex(true_clusters, clusters_kmeans)
#         }, error = function(e) 0, warning = function(w) 0)
#         rand_kmeans <- cbind(info, criterion = criterion,  scatter = scatter,
#                              method = "kmeans", ARI = ARI, eta2 = eta2,
#                              nb_select = nb_select)
# 
#         # tkmeans
#         ARI <- tryCatch({
#           clusters_tkmeans <- do.call("tkmeans_clust",
#                                       list(df = reduced_df, k = nb_clusters,
#                                            clusters_only = TRUE,  alpha = 0.05))
#           mclust::adjustedRandIndex(true_clusters, clusters_tkmeans)
#         }, error = function(e) 0, warning = function(w) 0)
#         rand_tkmeans <- cbind(info,criterion = criterion,  scatter = scatter,
#                               method = "tkmeans", ARI = ARI, eta2 = eta2,
#                               nb_select = nb_select)
#         # pam
#         ARI <- tryCatch({
#           clusters_pam <- do.call("pam_clust",
#                                   list(df = reduced_df, k = nb_clusters,
#                                        clusters_only = TRUE))
#           mclust::adjustedRandIndex(true_clusters, clusters_pam)
#         }, error = function(e) 0, warning = function(w) 0)
#         rand_pam <-  cbind(info, criterion = criterion,  scatter = scatter,
#                            method = "PAM", ARI = ARI, eta2 = eta2,
#                            nb_select = nb_select)
#         # combine results
#         rbind(rand_kmeans, rand_tkmeans, rand_pam)
# 
#       })
# 
#       # combine results from current simulation run into data frame
#       df_ARI_ICS <- do.call(rbind, results_ARI_ICS)
# 
# 
#       rbind(df_ARI_no_reduction, df_ARI_ICS)
#     })
# 
#     # combine results from current simulation run into data frame
#     df_ARI_ICS <- do.call(rbind, results_epsilon)
# 
#   })
# 
# 
#   # combine results from current simulation run into data frame
#   do.call(rbind, results_clusters)
# 
# }, mc.cores = n_cores)
# 
# # combine results into data frame
# results <- do.call(rbind, results_list)
# 
# # save results to file
# # file_results <- "results/results_n=%d.RData"
# # save(results, n, p, prob, rho, seed, file = sprintf(file_results, n))
# 
# # print message that simulation is done
# cat(paste(Sys.time(), ": finished.\n"))
# 
# 
# # aggregate results over the simulation runs
# library("dplyr")
# aggregated <- results %>%
#   group_by(epsilon, method) %>%
#   summarize(ARI = mean(ARI),
#             .groups = "drop")
# 
# # plot average results over the simulation runs
# library("ggplot2")
# p1 <- results %>% ggplot() +
#   geom_boxplot(aes(x = clusters, y = ARI, fill = method)) +
#   facet_grid(criterion ~factor(epsilon))
# p1
# # save plot to file
# # file_plot <- "figures/results_n=%d.pdf"
# # pdf(file = sprintf(file_plot, n), width = 5, height = 3.5)
# # print(p)
# # dev.off()

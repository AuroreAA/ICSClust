# # load packages, but functions should still be called with package::function()
# library("parallel")
# library("ICS")
# library("rrcov")
# 
# 
# # Parameters --------------------------------------------------------------
# 
# # control parameters for data generation
# n <- 1000                               # number of observations
# p <- 10                                 # number of variables
# delta <- 10                             # shift location
# R <- 2                                  # number of simulation runs
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
#                           c(0.90, 0.10),  c(0.95, 0.05),
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
# ICS_scatters_list <- list(
#   `COV-COV[4]` = c(S1 = ICS_cov, S2 = ICS_cov4),
#   `MLC-COV` = c(S1 = ICS_mlc, S2 = ICS_cov),
#   `LCOV-COV` = c(S1 = ICS_lcov, S2 = ICS_cov,
#                  S1_args = list(mscatter = "cov", proportion = 0.1)),
#   `TCOV-COV` = c(S1 = ICS_tcov, S2 = ICS_cov,
#                  S1_args = list(beta = 2)),
#   `TCOV-UCOV` = c(S1 = ICS_tcov, S2 = ICS_ucov,
#                   S1_args = list(beta = 2), S2_args = list(beta = 0.2)),
#   `MCD[0.25]-MCD[0.95]` = c(S1 = ICS_mcd, S2 = ICS_mcd,
#                             S1_args = list(alpha = 0.25),
#                             S2_args = list(alpha = 0.95)),
#   `MCD[0.10]-COV` = c(S1 = ICS_mcd, S2 = ICS_cov,
#                       S1_args = list(alpha = 0.10)),
#   `MCD[0.20]-COV` = c(S1 = ICS_mcd, S2 = ICS_cov,
#                       S1_args = list(alpha = 0.20)),
#   `MCD[0.25]-COV` = c(S1 = ICS_mcd, S2 = ICS_cov,
#                       S1_args = list(alpha = 0.25)),
#   `MCD[0.50]-COV` = c(S1 = ICS_mcd, S2 = ICS_cov,
#                       S1_args = list(alpha = 0.50)),
#   `MCD[0.75]-COV` = c(S1 = ICS_mcd, S2 = ICS_cov,
#                       S1_args = list(alpha = 0.75)),
#   `RMCD[0.25]-RMCD[0.95]` = c(S1 = ICS_rmcd, S2 = ICS_rmcd,
#                               S1_args = list(alpha = 0.25),
#                               S2_args = list(alpha = 0.95)),
#   `RMCD[0.10]-COV` = c(S1 = ICS_rmcd, S2 = ICS_cov,
#                        S1_args = list(alpha = 0.10)),
#   `RMCD[0.20]-COV` = c(S1 = ICS_rmcd, S2 = ICS_cov,
#                        S1_args = list(alpha = 0.20)),
#   `RMCD[0.25]-COV` = c(S1 = ICS_rmcd, S2 = ICS_cov,
#                        S1_args = list(alpha = 0.25)),
#   `RMCD[0.50]-COV` = c(S1 = ICS_rmcd, S2 = ICS_cov,
#                        S1_args = list(alpha = 0.50)),
#   `RMCD[0.75]-COV` =  c(S1 = ICS_rmcd, S2 = ICS_cov,
#                         S1_args = list(alpha = 0.75))
# )
# 
# ICS_scatters_list <- list(
#   `COV-COV[4]` = c(S1 = ICS_cov, S2 = ICS_cov4),
#   `MCD[0.25]-COV` = c(S1 = ICS_mcd, S2 = ICS_cov,
#                       S1_args = list(alpha = 0.25)),
#   `LCOV-COV` = c(S1 = ICS_lcov, S2 = ICS_cov,
#                  S1_args = list(mscatter = "cov", proportion = 0.1)),
#   `TCOV-COV` = c(S1 = ICS_tcov, S2 = ICS_cov,
#                  S1_args = list(beta = 2)))
# 
# 
# # control parameters for ICS criteria
# ICS_criteria <- c("normal_crit", "med_crit", "var_crit", "discriminatory_crit")
# ICS_criteria_args <- list(
#   normal_crit = list(level = 0.05,  test = "agostino.test"),
#   med_crit = list(nb_select = c()),
#   var_crit = list(nb_select = c()),
#   discriminatory_crit = list(clusters = vector(), nb_select = c())
# )
# 
# 
# # control parameters for PCA criteria
# PCA_criteria <- c("pct_crit", "min_crit")
# PCA_criteria_args <- list(
#   `80%` = c(pct = 0.8),
#   `k-1` = c(nb_select = c())
# )
# 
# min_crit <- function(object, nb_select = NULL){
#  colnames(object$scores)[0:nb_select]
# }
# 
# pct_crit <- function(object, pct = 0.8){
#   # select the names of the components which explains xxx pct of the total inertia
#   nc <- which(cumsum(object$eigenvalues) > pct*sum(object$eigenvalues))[1]
#   colnames(object$scores)[0:nc]
# }
# 
# # control parameters for clustering methods
# clustering <- c("kmeans_clust", "tkmeans_clust", "pam_clust")
# clustering_args <- list(
#   kmeans_clust = c(iter.max = 100, nstart = 20),
#   tkmeans_clust = c(alpha = 0.05),
#   pam_clust = c()
# )
# 
# # it is very easy to use parallel computing on Unix systems, but not on Windows
# if (.Platform$OS.type == "windows") {
#   n_cores <- 1              # use only one CPU core
# } else {
#   n_cores <- 2              # number of CPU cores to be used
#   RNGkind("L'Ecuyer-CMRG")  # use parallel random number streams
# }
# 
# # debug ----
# # r = 1
# # epsilon = 0
# # pct_clusters = c(0.5,0.5)
# # criterion = ICS_criteria[2]
# # i = 1
# 
# 
# # run simulation -----
# cat(paste(Sys.time(), ": starting ...\n"))
# set.seed(seed)
# results_list <- parallel::mclapply(seq_len(R), function(r) {
# 
#   # print simulation run
#   cat(paste(Sys.time(), sprintf(":   run = %d\n", r)))
# 
# 
#   # mixture weights --------------------------------------------------------------
#   # loop over different mixture weights
#   results_clusters <- lapply(pct_clusters_list, function(pct_clusters) {
#     # We simulate normal gaussian for each cluster with the first variable
#     # being true clusters
# 
#     data <- mixture_sim(pct_clusters = pct_clusters, n = n, p = p,
#                         delta = delta)
#     nb_clusters <- length(unique(data$cluster))
#     # define nb_select by default equals to the number of clusters -1
#     nb_select <- length(pct_clusters)-1
#     PCA_criteria_args$`k-1` <- c(nb_select = nb_select)
#     ICS_criteria_args$med_crit <- c(nb_select = nb_select)
#     ICS_criteria_args$var_crit <- c(nb_select = nb_select)
#     ICS_criteria_args$discriminatory_crit$nb_select <- nb_select
# 
# 
# 
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
#     ## epsilon - outliers ----
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
#       # Update some parameters for criteria
#       ICS_criteria_args$discriminatory_crit$clusters <- true_clusters
#       info <- data.frame(Run = r, epsilon = epsilon, n = n, p = p,
#                          delta = delta, q = length(pct_clusters),
#                          clusters = paste(round(pct_clusters*100),
#                                           collapse = "-"))
# 
#       # No Dimension reduction ----
#       # Additional info
#       criterion <- NA_character_
#       scatter <- "Observed~data"
#       selected <- paste(colnames(data[,-1]), collapse = ",")
# 
# 
#       results_ARI_no_reduction <- lapply(clustering, function(method) {
#         if (method == "kmeans_clust"){
#           reduced_df <- scale(data[,-1], center = TRUE, scale = TRUE)
#         }else{ # robustly center
#           reduced_df <- scale(data[,-1],
#                               center = apply(data[,-1], 2, median),
#                               scale = apply(data[,-1], 2, mad))
#         }
# 
#         nb_select <- ncol(reduced_df)
# 
#         # Compute discriminatory power
#         eta2 <- eta2_power(reduced_df, clusters = true_clusters,
#                            select = 1:nb_select)
# 
#         ARI <- tryCatch({
#           clusters <- do.call(method,
#                               append(list(df = reduced_df, k = nb_clusters,
#                                           clusters_only = TRUE),
#                                      clustering_args[[method]]))
#           mclust::adjustedRandIndex(true_clusters, clusters)
#         }, error = function(e) 0, warning = function(w) 0)
#         cbind(info, criterion = criterion, scatter = scatter,
#               method = gsub("_clust", "", method), ARI = ARI, eta2 = eta2,
#               nb_select = nb_select, selected = selected)
#       })
# 
#       # combine results
#       df_ARI_no_reduction <- do.call(rbind, results_ARI_no_reduction)
# 
#       # PCA ----
#       ## classical ----
#       PCA_out <- rrcov::PcaClassic(data[,-1], k = p, kmax = p, scale = TRUE)
#       scatter <- "COV"
#       ### criteria ----
#       results_ARI_PCA_crit <- lapply(1:length(PCA_criteria), function(i) {
#         # Select the components
#         criterion <- names(PCA_criteria_args)[i]
#         select <- do.call(PCA_criteria[i],
#                           append(list(object = PCA_out),
#                                  PCA_criteria_args[[i]]))
#         nb_select <- length(select)
# 
# 
#         # Compute discriminatory power
#         eta2 <-  tryCatch({eta2_power(PCA_out@scores,
#                                       clusters = true_clusters, select = select)
#         },error = function(e) 0, warning = function(w) 0)
#         reduced_df <- PCA_out@scores[,select, drop = FALSE]
#         selected <- paste(colnames(reduced_df), collapse = ",")
# 
#         # clustering ----
#         results_ARI_PCA <- lapply(clustering, function(method) {
#           ARI <- tryCatch({
#             clusters <- do.call(method,
#                                 append(list(df = reduced_df, k = nb_clusters,
#                                             clusters_only = TRUE),
#                                        clustering_args[[method]]))
#             mclust::adjustedRandIndex(true_clusters, clusters)
#           }, error = function(e) 0, warning = function(w) 0)
#           cbind(info, criterion = criterion, scatter = scatter,
#                 method = gsub("_clust", "", method), ARI = ARI, eta2 = eta2,
#                 nb_select = nb_select, selected = selected)
#         })
#         do.call(rbind, results_ARI_PCA)
#       })
# 
#       df_results_ARI_PCA <- do.call(rbind, results_ARI_PCA_crit)
# 
#       ## robust -----
#       rob_PCA_out <- rrcov::PcaCov(data[,-1], k = p, kmax = p, scale = TRUE,
#                             cov.control = rrcov::CovControlMcd(alpha = 0.75,
#                                                        nsamp = "deterministic"))
#       scatter <- "RMCD[0.75]"
#       ### criteria ----
#       results_ARI_rob_PCA_crit <- lapply(1:length(PCA_criteria), function(i) {
#         # Select the components
#         criterion <- names(PCA_criteria_args)[i]
#         select <- do.call(PCA_criteria[i],
#                           append(list(object = rob_PCA_out),
#                                  PCA_criteria_args[[i]]))
#         nb_select <- length(select)
# 
# 
#         # Compute discriminatory power
#         eta2 <-  tryCatch({eta2_power(PCA_out@scores,
#                                       clusters = true_clusters, select = select)
#         },error = function(e) 0, warning = function(w) 0)
#         reduced_df <- PCA_out@scores[, select, drop = FALSE]
#         selected <- paste(colnames(reduced_df), collapse = ",")
# 
#         # clustering ----
#         results_ARI_PCA <- lapply(clustering, function(method) {
#           ARI <- tryCatch({
#             clusters <- do.call(method,
#                                 append(list(df = reduced_df, k = nb_clusters,
#                                             clusters_only = TRUE),
#                                        clustering_args[[method]]))
#             mclust::adjustedRandIndex(true_clusters, clusters)
#           }, error = function(e) 0, warning = function(w) 0)
#           cbind(info, criterion = criterion, scatter = scatter,
#                 method = gsub("_clust", "", method), ARI = ARI, eta2 = eta2,
#                 nb_select = nb_select, selected = selected)
#         })
#         do.call(rbind, results_ARI_PCA)
#       })
# 
#       df_results_ARI_rob_PCA <- do.call(rbind, results_ARI_rob_PCA_crit)
# 
#       # ICS ----
#       ## scatters ------
#       results_ARI_ICS_scatters <- lapply(1:length(ICS_scatters_list),
#                                          function(i) {
# 
#         scatter = names(ICS_scatters_list)[i]
#         ICS_out <- do.call(ICS::ICS,
#                            append(list(X = data[,-1]), ICS_scatters_list[[i]]))
# 
#         ## criteria ----
#         results_ARI_ICS_crit <- lapply(ICS_criteria, function(criterion) {
#           # Select the components
#           select <- do.call(criterion, append(list(object = ICS_out,
#                                                    select_only = TRUE),
#                                               ICS_criteria_args[[criterion]]))
#           nb_select <- length(select)
# 
#           # Compute discriminatory power
#           eta2 <-  tryCatch({eta2_power(ICS::components(ICS_out),
#                                         clusters = true_clusters,
#                                         select = select)
#           },error = function(e) 0, warning = function(w) 0)
# 
#           reduced_df <- ICS::components(ICS_out, select = select)
#           selected <- paste(colnames(reduced_df), collapse = ",")
# 
#           # clustering ----
#           results_ARI_ICS_clust <- lapply(clustering, function(method) {
#             ARI <- tryCatch({
#               clusters <- do.call(method,
#                                   append(list(df = reduced_df, k = nb_clusters,
#                                               clusters_only = TRUE),
#                                          clustering_args[[method]]))
#               mclust::adjustedRandIndex(true_clusters, clusters)
#             }, error = function(e) 0, warning = function(w) 0)
#             cbind(info, criterion =  gsub("_crit", "", criterion),
#                   scatter = scatter, method = gsub("_clust", "", method),
#                   ARI = ARI, eta2 = eta2,
#                   nb_select = nb_select, selected = selected)
#           })
# 
#           do.call(rbind, results_ARI_ICS_clust)
#         })
# 
#         # combine results from current simulation run into data frame
#         do.call(rbind, results_ARI_ICS_crit)
#       })
# 
#       df_ARI_ICS <- do.call(rbind, results_ARI_ICS_scatters)
# 
#       rbind(df_ARI_no_reduction, df_results_ARI_PCA,  df_results_ARI_rob_PCA,
#             df_ARI_ICS)
#     })
# 
#     # combine results from current simulation run into data frame
#     do.call(rbind, results_epsilon)
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
# file_results <- "results/results_r=%d.RData"
# save(results, seed, file = sprintf(file_results, R))
# 
# # print message that simulation is done
# cat(paste(Sys.time(), ": finished.\n"))
# 
# 
# # plot average results over the simulation runs
# library("ggplot2")
# p1 <- results %>% ggplot() +
#   geom_boxplot(aes(x = scatter, y = ARI, fill = method)) +
#   facet_grid(criterion ~ clusters + factor(epsilon))
# p1
# 
# 
# 

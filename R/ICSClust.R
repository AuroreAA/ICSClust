ICSClust <- function(X, ICS_args = list(S1 = ICS_tcov, S2 = ICS_cov),
                     nb_select = NULL, 
                     criterion = c("med_crit", "normal_crit", "var_crit",
                                   "discriminatory_crit"), 
                     ICS_crit_args = list(),
                     clusters = NULL,
                     nb_clusters = NULL, 
                     method = c("kmeans_clust", "tkmeans_clust", "pam_clust"),
                     clustering_args = list()
){
  
  # Initialization
  criterion <- match.arg(criterion)
  method <- match.arg(method)

  
  if(is.null(nb_clusters)){
    stop("You should specify the `nb_clusters` argument.")
  }
  
  # ICS ----
  ICS_out <-  do.call(ICS::ICS, append(list(X = X), ICS_args))
  
  # Choice of components ----
  if(criterion %in% c("med_crit", "var_crit", "discriminatory_crit")){
    ICS_crit_args <- append(ICS_crit_args, c(nb_select = nb_select))
  }
  if (criterion %in% c("discriminatory_crit")){
    ICS_crit_args <- append(ICS_crit_args, list(clusters = clusters))
  }
  select <- do.call(criterion, append(list(object = ICS_out,
                                           select_only = TRUE),
                                      ICS_crit_args))
  
  # Clustering ----
  reduced_df <- ICS::components(ICS_out, select = select)
  selected <- paste(colnames(reduced_df), collapse = ",")
  clusters <- do.call(method,
                      append(list(df = reduced_df, k = nb_clusters,
                                  clusters_only = TRUE),
                             clustering_args))
  
  return(list(ICS_out = ICS_out, select = select, clusters = clusters))
}
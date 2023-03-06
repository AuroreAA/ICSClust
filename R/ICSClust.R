#' Title
#'
#' @param object Object of class 'ICS'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' @import ICS
#' @importFrom mclust adjustedRandIndex
ICSClust <- function(object, crit = "normal", crit_args = list(), groups_vec = NULL,
                     comp_max = NULL, scaling = FALSE, clustering_fun = "kmeansClust",
                     clustering_args = list(), nb_clusters = NULL, ...){
  
  if (class(object) != "ICS") stop("'object' must be of class ICS")
  
  # Selection of components
  ICS_comp <- ICS_comp_crit(object, crit = crit, crit_args = crit_args,
                            groups_vec = groups_vec, comp_max = comp_max)
  reduced_df <- object$scores[, ICS_comp$comp_selected]
  
  
  # Clustering
  if(scaling) reduced_df <- scale(reduced_df, center = TRUE, scale = TRUE)
  clustering_out <- do.call(clustering_fun, append(list(df = reduced_df, k = nb_clusters), 
                                        clustering_args))
  
  # Evaluation
  # Only rand index? multiple
  rand <- mclust::adjustedRandIndex(groups_vec, clustering_out$partition)
  
  
  out <- list(reduction = ICS_comp, clustering = append(clustering_out, list(ARI = rand)))
  class(out) <- "ICSClust"
  return(out)
}


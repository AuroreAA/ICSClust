#' Tandem clustering with ICS
#' 
#' Sequential clustering approach: (i) dimension reduction through the Invariant 
#' Coordinate Selection method using the \code{\link[=ICS-S3]{ICS}} function and (ii)
#' clustering of the transformed data. 
#'
#' @param X a numeric matrix or data frame containing the data.
#' @param nb_select the number of components to select. 
#' It is used only in case \code{criterion} is either "med_crit", "var_crit" or 
#' "discriminatory_crit".  By default it is set to \code{NULL}, i.e the number 
#' of components to select is the number of variables minus one.
#' @param nb_clusters the number of clusters searched for. 
#' @param ICS_args list of \code{\link[ICS]{ICS-S3}} arguments. Otherwise, default 
#' values of \code{\link[ICS]{ICS-S3}} are used.
#' @param criterion criterion to automatically decide which invariant components
#'  to keep. Possible values are "med_crit", "normal_crit", "var_crit" and 
#'  "discriminatory_crit". The default value is "med_crit". 
#'  See [med_crit()], [normal_crit()], [var_crit()] or 
#' [discriminatory_crit()] for more details.
#' @param ICS_crit_args list of arguments passed to [med_crit()], [normal_crit()],
#' [var_crit()] or 
#' [discriminatory_crit()] for choosing the components to keep.
#' @param method clustering method to perform. Possible implemented wrapper
#'  functions are named "kmeans_clust", "tkmeans_clust", "pam_clust",
#'  "mclust_clust" or "rimle_clust".
#'  The default value is "kmeans_clust".
#' @param clustering_args list of [kmeans_clust()], 
#' [tkmeans_clust()],  [pam_clust()], [rimle_clust()] or [mclust_clust()] 
#' arguments for performing cluster analysis.
#' @param clusters a vector indicating the true clusters of the data. By default,
#' it is \code{NULL} but it is required to choose the components based on the 
#' discriminatory criterion \code{\link{discriminatory_crit}}.
#'
#' @details
#' Tandem clustering with ICS is a sequential method:
#' 
#' - \code{\link[=ICS-S3]{ICS}} is performed.
#' 
#' - only a subset of the first and/or the last few components are
#'  selected based on a criterion.
#'  
#' - the clustering method is performed only on the subspace
#'  of the selected components.
#'  
#'
#' @return  
#' An object with the following components:
#' - `ICS_out`: An object of class \code{"ICS"}. 
#' See \code{\link[=ICS-S3]{ICS}}
#' - `select`: a vector of the names of the selected invariant
#'  coordinates.
#' - `clusters`: a vector of the new partition of the data, i.e a vector
#'  of integers (from \code{1:k}) indicating the cluster to which each
#'   observation is allocated. 0 indicates outlying observations.
#'   
#' `summary()` and `plot()` methods are available.
#'  
#' @references
#' Alfons, A., Archimbaud, A., Nordhausen, K., & Ruiz-Gazen, A. (2022). 
#' Tandem clustering with invariant coordinate selection. 
#' \emph{arXiv preprint arXiv:2212.06108}.
#' 
#' @export
#' 
#' @author Andreas Alfons and Aurore Archimbaud
#' 
#' @seealso [med_crit()], [normal_crit()], 
#' [var_crit()], \link[=ICS-S3]{ICS}, 
#' [discriminatory_crit()], [kmeans_clust()], [tkmeans_clust()], 
#' [pam_clust()], [rimle_clust()], [mclust_clust()]
#'
#' @examples
#' \dontrun{
#' X <- iris[,1:4]
#' 
#' # indicating the number of components to retain for the dimension reduction
#' # step as well as the number of clusters searched for.
#' out <- ICSClust(X, nb_select = 2, nb_clusters = 3)
#' summary(out)
#' plot(out)
#' 
#' # changing the scatter pair to consider in ICS
#' out <- ICSClust(X, nb_select = 1, nb_clusters = 3,
#' ICS_args = list(S1 = ICS_mcd, S2 = ICS_cov,S1_args = list(alpha = 0.5)))
#' summary(out)
#' plot(out)
#'  
#' # changing the criterion for choosing the invariant coordinates
#' out <- ICSClust(X, nb_clusters = 3, criterion = "normal_crit",
#' ICS_crit_args = list(level = 0.1, test = "anscombe.test", max_select = NULL))
#' summary(out)
#' plot(out)
#' 
#' # changing the clustering method
#' out <- ICSClust(X, nb_clusters = 3, method  = "tkmeans_clust", 
#' clustering_args = list(alpha = 0.1))
#' summary(out)
#' plot(out)
#'  }
ICSClust <- function(X, nb_select = NULL, nb_clusters = NULL, ICS_args = list(),
                     criterion = c("med_crit", "normal_crit", "var_crit",
                                   "discriminatory_crit"), 
                     ICS_crit_args = list(),
                     method = c("kmeans_clust", "tkmeans_clust", "pam_clust",
                                "mclust_clust", "rimle_clust"),
                     clustering_args = list(),
                     clusters = NULL
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
  
  out <- list(ICS_out = ICS_out, select = select, clusters = clusters)
  class(out) <- "ICSClust"
  out
}


#' @method summary ICSClust
#' @export
summary.ICSClust <- function(object, info = FALSE, digits = 4L, ...) {
  # print information on ICS
  print(object$ICS_out, info = info, digits = digits)
  
  # print information on selected components
  cat("\n", length(object$select), "components are selected:", object$select)
  
  # print information on clusters
  cat("\n\n", length(unique(object$clusters)), "clusters are identified:\n")
  print(table(object$clusters))
  
  # return object invisibly
  invisible(object)
}

#' @method plot ICSClust
#' @export
plot.ICSClust <- function(object, ...) {
  component_plot(object$ICS_out, select = object$select, 
                 clusters = factor(object$clusters), ...)
}
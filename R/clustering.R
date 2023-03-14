#' Kmeans clustering
#' 
#'
#' @param df 
#' @param k 
#' @param clusters_only 
#' @param iter.max 
#' @param nstart 
#' @param ... 
#'
#' @return
#' @export
#' 
#' @importFrom stats kmeans
#'
#' @examples
kmeans_clust <- function(df, k, clusters_only = FALSE, iter.max = 100, nstart = 20, ...){
  clust <- kmeans(x = df, centers = k, iter.max = iter.max, nstart = nstart, ...)
  out = clust$cluster
  if (!clusters_only) out <- append(list(clust_method = "kmeans", clusters = out), clust)
  out
}


#' Title
#'
#'
#' @param df 
#' @param k 
#' @param clusters_only 
#' @param alpha 
#' @param ... 
#'
#' @return
#' @export
#' 
#' @importFrom tclust tkmeans
#'
#' @examples
tkmeans_clust <- function(df, k, clusters_only = FALSE, alpha = 0.05, ... ){
  clust <- tclust::tkmeans(x = df, k = k, alpha = alpha, ...)
  # the additional cluster of outliers are coded by 0
  out = clust$cluster
  if (!clusters_only) out <- append(list(clust_method = "tkmeans", clusters = out), clust)
  out
}

#' Title
#'
#'
#' @param df 
#' @param k 
#' @param clusters_only 
#' @param diss 
#' @param .. 
#'
#' @return
#' @export
#' 
#' @importFrom cluster pam
#'
#' @examples
pam_clust <- function(df, k, clusters_only = FALSE, diss = inherits(df,"dist"), ...){
  clust <- cluster::pam(x = df, k = k, diss = diss, ...)
  # the additional cluster of outliers are coded by 0
  out = clust$cluster
  if (!clusters_only) out <- append(list(clust_method = "clara_pam", clusters = out), clust)
  out
}
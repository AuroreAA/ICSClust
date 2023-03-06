tandemClust <- function(X, reduction = c("none", "PCA", "ICS"), 
                        clustering = c("PAM", "kmeans", "tkmeans")){
  # Dimension reduction
  
  # output: reduced df 
  
  # Clustering
  #  scale(data,center=TRUE,scale=scaling
  
}



#' Kmeans clustering
#' 
#' The output is of class tandemClust
#'
#' @param x 
#' @param centers 
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
kmeansClust <- function(df, k, iter.max = 100, nstart = 20, ...){
  # kmeans clustering
  clust <- kmeans(x = df, centers = k, iter.max = iter.max, nstart = nstart, ...)
  out <- list(partition = clust$cluster, clustermethod = "kmeans")
  class(out) <- "tandemClust"
  out
}


#' Title
#'
#' @param df 
#' @param k 
#' @param alpha 
#' @param ... 
#'
#' @return
#' @export
#' 
#' @importFrom tclust tkmeans
#'
#' @examples
tkmeansClust <- function(df, k,  alpha = 0.05, ... ){
  clust <- tclust::tkmeans(x = df, k = k, alpha = alpha, ...)
  # the additional cluster of outliers are coded by 0
  out <- list(partition = clust$cluster, clustermethod = "tkmeans")
  class(out) <- "tandemClust"
  out
}

#' Title
#'
#' @param df 
#' @param k 
#' @param usepam 
#' @param diss 
#'
#' @return
#' @export
#' 
#' @importFrom cluster pam
#'
#' @examples
pamClust <- function(df, k, diss = inherits(df,"dist"), ..){
  clust <- cluster::pam(df, k = k, diss = diss, ...)
  # the additional cluster of outliers are coded by 0
  out <- list(partition = clust$clustering, clustermethod = "clara/pam")
  class(out) <- "tandemClust"
  out
}
#' Mixture of gaussian distributions
#'
#' @param pct_clusters 
#' @param n 
#' @param p 
#' @param delta 
#' @param sigma 
#' @param mean_grp 
#' @param outliers 
#' @param eps 
#'
#' @return
#' @export
#' 
#' @importFrom mvtnorm rmvnorm
#'
#' @examples
mixture_sim = function(pct_clusters = c(0.5,0.5) , n = 500, p = 10, delta = 10){
  
  # Checks and initialization of inputs
  if(sum(pct_clusters)!=1){
    stop("the sum of the groups is not equal to 1!") 
  }
  n_groups = floor(n*pct_clusters)
  if(sum(n_groups)!= n){
    n_groups[length(pct_clusters)] <- 
      rev(n-cumsum(n_groups)[1:(length(pct_clusters)-1)])[1]
    
  }
  if(sum(n_groups)!= n){
    warning(paste("the total number of observation is not equal to n",
                  paste(round(pct_clusters,2), collapse = " - ")))
  }
 
  # We simulate normal gaussian for each cluster
  clusters_means <- rep(0,p)
  X_list <- lapply(1:length(pct_clusters), function(i){
    n <-  n_groups[i]
    # we introduce the shift location 
    if(i>1){
      clusters_means[i-1] = delta
    }
    if(n>0){
      data.frame(mvtnorm::rmvnorm(n = n, mean = clusters_means, 
                                  sigma = diag(1,p)))
    }
  })
  
  data.frame(cluster = rep(paste0("Group",  1:length(pct_clusters)), n_groups),
                    do.call(rbind, (X_list)))
}

#' Outside range
#' 
#' Draw from a multivariate uniform distribution outside range of a data set
#'
#' @param n number of observations to generate
#' @param min numeric vector giving the minimum of each variable of the data set
#' @param max numeric vector giving the maximum of each variable of the data set
#' @param mult multiplication factor to expand the hyperrectangle around the data 
#'            (which is given by 'min' and 'max'), e.g., the default value 2 gives 
#'            a hyperrectangle for which each side is twice as long.  The data are 
#'            then drawn from a uniform distribution on the expanded hyperrectangle 
#'            from which the smaller hyperrectangle around the data is cut out.
#' @return
#' @export
#'
#' @examples
runif_outside_range <- function(n, min = 0, max = 1, mult = 2) {
  # center and dimension of hyperrectangles
  m <- (max + min) / 2
  p <- length(m)
  # length of sides of hyperrectangle around data
  a <- max - min
  # halved length of sides of expanded hyperrectangle
  h <- mult * a / 2
  # lower and upper bounds of expanded hyperrectangle
  lower <- m - h
  upper <- m + h
  # draw from uniform distribution on expanded hyperrectangle and reject the 
  # observation if it lies in the smaller hyperrectangle around the data
  n_ok <- 0
  X <- matrix(NA_real_, nrow = n, ncol = p)
  while (n_ok < n) {
    # draw new point from uniform distribution on expanded hyperrectangle
    x <- runif(p, min = lower, max = upper)
    # reject if it lies inside of the smaller hyperrectangle around the data
    reject <- all(x >= min & x <= max)
    # add to matrix
    if (!reject) {
      n_ok <- n_ok + 1
      X[n_ok, ] <- x
    }
  }
  # return matrix of generated points
  X
}
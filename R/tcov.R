# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************

#' @export
tcov1 <- function(x, beta = 4) {
  n <- nrow(x)
  cov_inv <- solve(stats::var(x)*(n-1)/n) 
  amap::W(x, h = 1/sqrt(beta), D = cov_inv, kernel = "gaussien")
}

#' @useDynLib ICSClust, .registration = TRUE
#' @export
tcov2 <- function(x, beta = 4) {
  # initializations
  x <- as.matrix(x)
  cn <- colnames(x)
  # call internal function (which in turn calls C++ function)
  V <- tcovCpp(x, beta = beta)
  # set row and column names and return scatter matrix
  dimnames(V) <- list(cn, cn)
  V
}

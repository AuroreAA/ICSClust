# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************

#' @export
ICS_tcov <- function(x, beta = 4) {
  # compute scatter estimate
  out <- list(location = NULL, scatter = tcov(x, beta = beta), label = "TCOV")
  # add class and return object
  class(out) <- "ICS_scatter"
  out
}

#' @useDynLib ICSClust, .registration = TRUE
#' @export
tcov <- function(x, beta = 4) {
  # initializations
  x <- as.matrix(x)
  cn <- colnames(x)
  # call internal function (which in turn calls C++ function)
  V <- tcovCpp(x, beta = beta)
  # set row and column names and return scatter matrix
  dimnames(V) <- list(cn, cn)
  V
}

## reference implementation using package 'amap'
## @importFrom stats var
## @importFrom amap W
## @export
# tcov_amap <- function(x, beta = 4) {
#   # initializations
#   x <- as.matrix(x)
#   cn <- colnames(x)
#   # compute inverse of maximum likelihood estimate of covariance matrix
#   n <- nrow(x)
#   cov_inv <- solve(var(x) * (n-1) / n)
#   V <- amap::W(x, h = 1/sqrt(beta), D = cov_inv, kernel = "gaussien")
#   # set row and column names and return scatter matrix
#   dimnames(V) <- list(cn, cn)
#   V
# }

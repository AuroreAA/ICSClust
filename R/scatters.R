# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************


## TCOV scatter matrix

#' @export
ICS_tcov <- function(x, beta = 2) {
  # compute scatter estimate
  out <- list(location = NULL, scatter = tcov(x, beta = beta), label = "TCOV")
  # add class and return object
  class(out) <- "ICS_scatter"
  out
}

#' @useDynLib ICSClust, .registration = TRUE
#' @export
tcov <- function(x, beta = 2) {
  # initializations
  x <- as.matrix(x)
  cn <- colnames(x)
  # call internal function (which in turn calls C++ function)
  V <- tcovCpp(x, beta = beta)
  # set row and column names and return scatter matrix
  dimnames(V) <- list(cn, cn)
  V
}

# reference implementation using package 'amap'
#' @importFrom amap W
#' @importFrom stats var
tcov_amap <- function(x, beta = 2) {
  # initializations
  x <- as.matrix(x)
  cn <- colnames(x)
  # compute inverse of maximum likelihood estimate of covariance matrix
  n <- nrow(x)
  cov_inv <- solve(var(x) * (n-1) / n)
  V <- amap::W(x, h = 1/sqrt(beta), D = cov_inv, kernel = "gaussien")
  # set row and column names and return scatter matrix
  dimnames(V) <- list(cn, cn)
  V
}


## SCOV scatter matrix

#' @export
ICS_scov <- function(x, beta = 0.2) {
  # compute location and scatter estimates
  location <- mean(x)
  scatter <- .scov(x, m = location, beta = beta)
  out <- list(location = location, scatter = scatter, label = "SCOV")
  # add class and return object
  class(out) <- "ICS_scatter"
  out
}

#' @export
scov <- function(x, beta = 0.2) {
  # initializations
  x <- as.matrix(X)
  # compute sample means and call internal function
  .scov(x, m = colMeans(x), beta = beta)
}

## internal function to avoid recomputation of sample mean
#' @useDynLib ICSClust, .registration = TRUE
.scov <- function(x, m, beta = 0.2) {
  # initializations
  cn <- colnames(x)
  # call internal function (which in turn calls C++ function)
  V <- scovCpp(x, m = m, beta = beta)
  # set row and column names and return scatter matrix
  dimnames(V) <- list(cn, cn)
  V
}


## UCOV scatter matrix

#' @export
ICS_ucov <- function(x, beta = 0.2) {
  # compute location and scatter estimates
  location <- mean(x)
  scatter <- .ucov(x, m = location, beta = beta)
  out <- list(location = location, scatter = scatter, label = "UCOV")
  # add class and return object
  class(out) <- "ICS_scatter"
  out
}

#' @export
ucov <- function(x, beta = 0.2) {
  # initializations
  x <- as.matrix(X)
  # compute sample means and call internal function
  .ucov(x, m = colMeans(x), beta = beta)
}

# internal function to avoid recomputation of sample mean
.ucov <- function(x, m, beta = 0.2) {
  # initializations
  n <- nrow(x)
  # compute inverse of SCOV and inverse of sample covariance matrix
  cov_inv <- solve(var(x) * (n-1) / n)
  # TODO: make the inverse of the covariance matrix an argument of .scov()
  #       (avoids computing it twice)
  scov_inv <- solve(.scov(x, m = m, beta = beta))
  # compute UCOV scatter matrix
  solve(scov_inv - beta * cov_inv)
}

## reference implementation using package 'amap'
#' @importFrom amap W
#' @importFrom stats var
ucov_amap <- function(x, beta = 0.2) {
  # TODO: Is this correct? I get slightly different numbers than with ucov().
  # initializations
  x <- as.matrix(x)
  cn <- colnames(x)
  # compute inverse of maximum likelihood estimate of covariance matrix
  n <- nrow(x)
  cov_inv <- solve(var(x) * (n-1) / n)
  # compute inverse of SCOV (error in amap package: should be 1/h^2)
  h <- 1/sqrt(beta)
  scov_inv <- solve(amap::varrob(x, h = h, D = cov_inv, kernel = "gaussien"))
  V <- solve(scov_inv + (h-1)/(h^2) * cov_inv)
  # set row and column names and return scatter matrix
  dimnames(V) <- list(cn, cn)
  V
}

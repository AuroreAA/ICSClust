# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************


# TCOV scatter matrix -----

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
  V <- tcov_cpp(x, beta = beta)
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
  # n <- nrow(x)
  # S_inv <- solve(var(x) * ((n-1)/n))
  S_inv <- solve(var(x))
  V <- amap::W(x, h = 1/sqrt(beta), D = S_inv, kernel = "gaussien")
  # set row and column names and return scatter matrix
  dimnames(V) <- list(cn, cn)
  V
}


# SCOV scatter matrix -----

#' @export
ICS_scov <- function(x, beta = 0.2) {
  # initializations
  x <- as.matrix(x)
  n <- nrow(x)
  # compute location and scatter estimates
  location <- colMeans(x)
  S_inv <- solve(var(x))
  scatter <- .scov(x, m = location, S_inv = S_inv, beta = beta)
  out <- list(location = location, scatter = scatter, label = "SCOV")
  # add class and return object
  class(out) <- "ICS_scatter"
  out
}

#' @export
scov <- function(x, beta = 0.2) {
  # initializations
  x <- as.matrix(x)
  n <- nrow(x)
  # compute sample means and inverse of covariance matrix
  m <- colMeans(x)
  S_inv <- solve(var(x))
  # call internal function
  .scov(x, m = m, S_inv = S_inv, beta = beta)
}

## internal function to avoid recomputation of sample mean
#' @useDynLib ICSClust, .registration = TRUE
.scov <- function(x, m, S_inv, beta = 0.2) {
  # initializations
  cn <- colnames(x)
  # call internal function (which in turn calls C++ function)
  V <- scov_cpp(x, m = m, S_inv = S_inv, beta = beta)
  # set row and column names and return scatter matrix
  dimnames(V) <- list(cn, cn)
  V
}


# UCOV scatter matrix -----

#' @export
ICS_ucov <- function(x, beta = 0.2) {
  # initializations
  x <- as.matrix(x)
  # compute location and scatter estimates
  location <- colMeans(x)
  scatter <- .ucov(x, m = location, beta = beta)
  out <- list(location = location, scatter = scatter, label = "UCOV")
  # add class and return object
  class(out) <- "ICS_scatter"
  out
}

#' @export
ucov <- function(x, beta = 0.2) {
  # initializations
  x <- as.matrix(x)
  # compute sample means
  m <- colMeans(x)
  # call internal function
  .ucov(x, m = m, beta = beta)
}

# internal function to avoid recomputation of sample mean
.ucov <- function(x, m, beta = 0.2) {
  # initializations
  n <- nrow(x)
  # compute inverse of sample covariance matrix and inverse of SCOV
  S_inv <- solve(var(x))
  scov_inv <- solve(.scov(x, m = m, S_inv = S_inv, beta = beta))
  # compute UCOV scatter matrix
  solve(scov_inv - beta * S_inv)
}

## reference implementation using package 'amap'
#' @importFrom amap W
#' @importFrom stats var
ucov_amap <- function(x, beta = 0.2) {
  # initializations
  x <- as.matrix(x)
  cn <- colnames(x)
  # compute inverse of maximum likelihood estimate of covariance matrix
  # n <- nrow(x)
  # S_inv <- solve(var(x) * ((n-1) / n))
  S_inv <- solve(var(x))
  # compute inverse of SCOV (error in 'amap' package: should be 1/h^2)
  h <- 1/sqrt(beta)
  scov_inv <- solve(amap::varrob(x, h = h, D = S_inv, kernel = "gaussien"))
  V <- solve(scov_inv + (h-1)/(h^2) * S_inv)
  # set row and column names and return scatter matrix
  dimnames(V) <- list(cn, cn)
  V
}

/*
 * Author: Andreas Alfons
 *         Erasmus Universiteit Rotterdam
 */

#include <RcppArmadillo.h>  // for covariance matrix and inverse
#include <math.h>           // for exponential function

using namespace Rcpp;
using namespace arma;

// TCOV scatter matrix
// [[Rcpp::export]]
arma::mat tcovCpp(const arma::mat& x, const double& beta) {
  
  // initializations
  const arma::uword n = x.n_rows;
  const arma::uword p = x.n_cols;
  
  // In the paper, we have w(x) = exp(-x/2). But since we always call 
  // w(beta * r^2), we instead set b = -beta/2 and use w(x) = exp(x).
  const double b = -beta / 2.0;
  
  // compute inverse of the covariance matrix
  // (second argument in arma::cov() specifies denominator n)
  const arma::mat S_inv = arma::solve(arma::cov(x, 1), 
                                      arma::eye<arma::mat>(p, p));
  
  // loop over pairs of observations
  arma::uword i, j, k, l;             // running indices
  arma::vec diff(p);                  // difference of pair of observations
  arma::mat V(p, p, fill::zeros);     // scatter matrix
  double r_sq, w, denominator = 0.0;  // squared distance, weight, denominator
  for(i = 1; i < n; i++) {
    for(j = 0; j < i; j++) {
      // compute difference of current pair of observations
      for(k = 0; k < p; k++) diff(k) = x(i,k) - x(j,k);
      // compute squared pairwise Mahalanobis distance
      r_sq = 0.0;
      for(k = 0; k < p; k++) {
        for(l = 0; l < p; l++) {
          r_sq += diff(k) * S_inv(k,l) * diff(l);
        }
      }
      // compute weight for current pair of observations
      w = exp(b * r_sq);
      // add weighted contribution of current pair of observations
      for(k = 0; k < p; k++) {
        // update diagonal elements
        V(k,k) += w * diff(k) * diff(k);
        // update offdiagonal elements
        for(l = 0; l < k; l++) {
          V(k,l) += w * diff(k) * diff(l);
          V(l,k) = V(k,l);
        }
      }
      // add weight of current pair of observations to denominator
      denominator += w;
    }
  }

  // return scatter matrix
  return V / denominator;
}

// SCOV scatter matrix
// [[Rcpp::export]]
arma::mat scovCpp(const arma::mat& x, const arma::vec& m, 
                  const double& beta) {
  
  // initializations
  const arma::uword n = x.n_rows;
  const arma::uword p = x.n_cols;
  
  // In the paper, we have w(x) = exp(-x/2). But since we always call 
  // w(beta * r^2), we instead set b = -beta/2 and use w(x) = exp(x).
  const double b = -beta / 2.0;
  
  // compute inverse of the covariance matrix
  // (second argument in arma::cov() specifies denominator n)
  const arma::mat S_inv = arma::solve(arma::cov(x, 1), 
                                      arma::eye<arma::mat>(p, p));
  
  // loop over pairs of observations
  arma::uword i, k, l;                // running indices
  arma::vec diff(p);                  // difference of pair of observations
  arma::mat V(p, p, fill::zeros);     // scatter matrix
  double r_sq, w, denominator = 0.0;  // squared distance, weight, denominator
  for(i = 1; i < n; i++) {
    // compute difference of current pair of observations
    for(k = 0; k < p; k++) diff(k) = x(i,k) - m(k);
    // compute squared Mahalanobis distance
    r_sq = 0.0;
    for(k = 0; k < p; k++) {
      for(l = 0; l < p; l++) {
        r_sq += diff(k) * S_inv(k,l) * diff(l);
      }
    }
    // compute weight for current pair of observations
    w = exp(b * r_sq);
    // add weighted contribution of current pair of observations
    for(k = 0; k < p; k++) {
      // update diagonal elements
      V(k,k) += w * diff(k) * diff(k);
      // update offdiagonal elements
      for(l = 0; l < k; l++) {
        V(k,l) += w * diff(k) * diff(l);
        V(l,k) = V(k,l);
      }
    }
    // add weight of current pair of observations to denominator
    denominator += w;
  }
  
  // return scatter matrix
  return V / denominator;
}

// // SCOV scatter matrix
// // [[Rcpp::export]]
// arma::mat scovCpp(const arma::mat& x, const double& beta) {
// 
//   // initializations
//   const arma::uword n = x.n_rows;
//   const arma::uword p = x.n_cols;
// 
//   // In the paper, we have w(x) = exp(-x/2). But since we always call
//   // w(beta * r^2), we instead set b = -beta/2 and use w(x) = exp(x).
//   const double b = -beta / 2.0;
// 
//   // compute sample means (second argument specifies to compute column means)
//   arma::rowvec m = arma::mean(x, 0);
// 
//   // center the data matrix by subtracting sample means
//   arma::uword i, j;
//   arma::mat x_centered(n, p);
//   double *m_j;
//   for(j = 0; j < p; j++) {
//     m_j = &m(j);
//     for(i = 0; i < n; i++) x_centered(i,j) = x(i,j) - *m_j;
//   }
// 
//   // compute sample covariance matrix with denominator n
//   arma::uword k;
//   double tmp;
//   arma::mat S(p, p);
//   for(i = 0; i < p; i++) {
//     for(j = 0; j <= i; j++) {
//       tmp = 0;
//       for(k = 0; k < n; k++) tmp += x_centered(k,i) * x_centered(k,j);
//       S(i,j) = tmp / (double)n;
//       S(j,i) = S(i,j);
//     }
//   }
// 
//   // compute inverse of the covariance matrix
//   const arma::mat S_inv = arma::solve(S, arma::eye<arma::mat>(p, p));
// 
//   // loop over pairs of observations
//   arma::uword l;                      // additional running index
//   arma::vec diff(p);                  // difference of pair of observations
//   arma::mat V(p, p, fill::zeros);     // scatter matrix
//   double r_sq, w, denominator = 0.0;  // squared distance, weight, denominator
//   for(i = 1; i < n; i++) {
//     // compute difference of current pair of observations
//     for(k = 0; k < p; k++) diff(k) = x(i,k) - m(k);
//     // compute squared Mahalanobis distance
//     r_sq = 0.0;
//     for(k = 0; k < p; k++) {
//       for(l = 0; l < p; l++) {
//         r_sq += diff(k) * S_inv(k,l) * diff(l);
//       }
//     }
//     // compute weight for current pair of observations
//     w = exp(b * r_sq);
//     // add weighted contribution of current pair of observations
//     for(k = 0; k < p; k++) {
//       // update diagonal elements
//       V(k,k) += w * diff(k) * diff(k);
//       // update offdiagonal elements
//       for(l = 0; l < k; l++) {
//         V(k,l) += w * diff(k) * diff(l);
//         V(l,k) = V(k,l);
//       }
//     }
//     // add weight of current pair of observations to denominator
//     denominator += w;
//   }
// 
//   // return scatter matrix
//   return V / denominator;
// }

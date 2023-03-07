/*
 * Author: Andreas Alfons
 *         Erasmus Universiteit Rotterdam
 */

#include <RcppArmadillo.h>
#include <math.h>

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
arma::mat tcovCpp(const arma::mat& x, const double& beta) {
  
  // FIXME: the transpose always creates a copy, hence this code is too slow
  
  // initializations
  const int n = x.n_rows;
  const int p = x.n_cols;
  
  // transpose the data matrix so that we can easily extract observations as 
  // column vectors and avoid making copies
  const arma::mat tx = x.t();

  // In the paper, we have w(x) = exp(-x/2). But since we always call 
  // w(beta * r^2), we instead set b = beta/2 and use w(x) = exp(-x).
  const double b = beta / 2.0;
  
  // compute covariance matrix and its inverse
  const arma::mat S = arma::cov(x, 1);  // use denominator n instead of n-1
  const arma::mat SInv = arma::solve(S, arma::eye<arma::mat>(p, p));
  
  // loop over pairs of observations
  arma::mat V(p, p); V.zeros();
  arma::vec diff(p);
  arma::rowvec tdiff(p);
  double rSq, w, denominator = 0;
  for(arma::uword i = 0; i < n-1; i++) {
    for (arma::uword j = i+1; j < n; j++) {
      diff = tx.unsafe_col(i) - tx.unsafe_col(j);
      tdiff = diff.t();
      rSq = arma::as_scalar(tdiff * SInv * diff);
      w = exp(-b*rSq);
      V = V + (w * (diff * tdiff));
      denominator += w;
    }
  }

  // return scatter matrix
  return V / denominator;
}

double w(const double& x) {
  return exp(-x);
}

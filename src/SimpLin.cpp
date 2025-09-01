#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace arma;

// [[Rcpp::export]]
Rcpp::List SimpLinCpp(const arma::vec& x, const arma::vec& y) {
  arma::mat X = arma::join_rows(arma::ones(y.size()), x);
  arma::vec bHat = arma::inv(X.t()*X)*X.t()*y;
  arma::vec yHat = X*bHat;
  arma::vec resid = y - yHat;
  double MSE = (arma::as_scalar(resid.t() * resid))/(y.size() - 2);
  arma::mat varBhat = MSE*inv(X.t()*X);
  double varBhat0 = varBhat(0,0);
  double varBhat1 = varBhat(1,1);
  double tCrit = R::qt(0.975, y.size() - 2, 1, 0);
  arma::rowvec B0_CI = {bHat[0] - sqrt(varBhat0)*tCrit,
                                   bHat[0] + sqrt(varBhat0)*tCrit};
  arma::rowvec B1_CI = {bHat[1] - sqrt(varBhat1)*tCrit,
                     bHat[1] + sqrt(varBhat1)*tCrit};
  return Rcpp::List::create(
    Rcpp::Named("coefficients") = bHat,
    Rcpp::Named("SE B0") = sqrt(varBhat0),
    Rcpp::Named("SE B1") = sqrt(varBhat1),
    Rcpp::Named("95% CI B0") = B0_CI,
    Rcpp::Named("95% CI B1") = B1_CI,
    Rcpp::Named("y hat") = yHat,
    Rcpp::Named("residuals") = resid
  );
}



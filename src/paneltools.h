#ifndef __PANELTOOLS_H__
#define __PANELTOOLS_H__


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
#include <RcppArmadillo.h>
using namespace Rcpp ;

arma::mat data_ub_adj(arma::mat I_data, arma::mat data);

#endif

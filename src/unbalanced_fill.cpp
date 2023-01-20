#include "paneltools.h"

/* adjust unbalanced data */
// [[Rcpp::export]]
arma::mat data_ub_adj (arma::mat I_data, arma::mat data) {
  int count = I_data.n_rows ;
  //int total = data.n_rows ;
  int nov = data.n_cols ;
  arma::mat data_adj(count,nov) ;
  data_adj.fill(arma::datum::nan) ;
  int j = 0;
  for(int i=0; i<count; i++){
    if(I_data(i,0)==1){
      data_adj.row(i) = data.row(j);
      j++;
    }
  }
  return(data_adj);
}
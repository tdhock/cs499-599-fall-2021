#include <Rcpp.h>
#include "find_closest_center.h"
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector find_closest_center_interface
(NumericMatrix data_mat, NumericMatrix centers_mat) {
  int N_data = data_mat.nrow();
  int N_centers = centers_mat.nrow();
  int N_features = centers_mat.ncol();
  if(N_features != data_mat.ncol()){
    Rcpp::stop("number of columns in data and centers should be the same");
  }
  double *data_ptr = &data_mat[0];
  double *centers_ptr = &centers_mat[0];
  IntegerVector cluster_vec(N_data);
  int *cluster_ptr = &cluster_vec[0];
  int status = find_closest_center
    (N_data,
     N_centers,
     N_features,
     data_ptr,
     centers_ptr,
     //inputs above, outputs below.
     cluster_ptr);
  if(status == ERROR_N_DATA_MUST_BE_POSITIVE){
    Rcpp::stop("N_data must be postiive");
  }
  if(status == ERROR_N_CENTERS_MUST_BE_POSITIVE){
    Rcpp::stop("N_centers must be postiive");
  }
  return cluster_vec;
}

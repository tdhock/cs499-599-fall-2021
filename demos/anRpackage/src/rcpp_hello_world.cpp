
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector rcpp_hello_world
(NumericMatrix data_mat, NumericMatrix centers_mat) {
  int N_data = data_mat.nrow();
  IntegerVector cluster_vec(N_data);
  for(int data_i=0; data_i<N_data; data_i++){
    double min_error = INFINITY;
    for(int center_i=0;
	center_i<centers_mat.nrow(); center_i++){
      double error = 0;
      for(int feature_i=0;
	  feature_i<centers_mat.ncol(); feature_i++){
	double diff =
	  data_mat(data_i, feature_i) -
	  centers_mat(data_i, feature_i);
	error += diff * diff;
      }
      if(error < min_error){
	min_error = error;
	cluster_vec[data_i] = center_i+1;
      }
    }
  }
  return cluster_vec;
}

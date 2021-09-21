
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector find_closest_center
(NumericMatrix data_mat, NumericMatrix centers_mat) {
  int N_data = data_mat.nrow();
  int N_centers = centers_mat.nrow();
  int N_features = centers_mat.ncol();
  if(N_features != data_mat.ncol()){
    //ERROR TODO
  }
  double *data_ptr = &data_mat[0];
  double *centers_ptr = &centers_mat[0];
  IntegerVector cluster_vec(N_data);
  int *cluster_ptr = &cluster_vec[0];
  for(int data_i=0; data_i<N_data; data_i++){
    double min_error = INFINITY;
    for(int center_i=0;
	center_i<N_centers; center_i++){
      double error = 0;
      for(int feature_i=0;
	  feature_i<N_features; feature_i++){
	double data_value =
	  data_ptr[feature_i*N_data + data_i];
	double center_value =
	  centers_ptr[feature_i*N_centers + center_i];
	double diff = data_value - center_value;
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

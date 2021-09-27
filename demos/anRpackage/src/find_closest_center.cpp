#include "find_closest_center.h"
#include <math.h>

int find_closest_center
(const int N_data,
 const int N_centers,
 const int N_features,
 const double *data_ptr,
 const double *centers_ptr,
 //inputs above, outputs below.
 int *cluster_ptr
 ){
  if(N_data < 1){
    return ERROR_N_DATA_MUST_BE_POSITIVE;
  }
  if(N_centers < 1){
    return ERROR_N_CENTERS_MUST_BE_POSITIVE;
  }
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
	cluster_ptr[data_i] = center_i+1;
      }
    }
  }
  return 0;//SUCCESS
}

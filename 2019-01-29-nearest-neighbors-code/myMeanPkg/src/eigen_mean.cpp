#include <Eigen/Dense>
#include "eigen_mean.h"

int eigen_mean_Cpp(double *data_ptr, int data_count, double *output_ptr){
  if(data_count==0){
    return EIGEN_MEAN_ERROR_NO_DATA;
  }
  Eigen::Map< Eigen::VectorXd > data_vec(data_ptr, data_count);
  *output_ptr = data_vec.mean();
  return 0;
}

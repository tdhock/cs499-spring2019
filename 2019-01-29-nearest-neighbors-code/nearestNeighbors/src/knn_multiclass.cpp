/* -*- compile-command: "R CMD INSTALL .. && R --vanilla < ../tests/testthat/test-knn-multiclass.R" -*- */

#include "knn_multiclass.h"
#include <Eigen/Dense>
#include <iostream>
#include <omp.h>

int Predict1toMaxNeighborsMultiClass
(double *train_inputs_ptr, int *train_label_ptr,
 int nrow, int ncol, int max_neighbors, int n_labels,
 double *distance_ptr,
 int *sorted_index_ptr,
 int *label_count_ptr,
 double *test_input_ptr,     // ncol
 int *test_prediction_ptr // max_neighbors
 ){
  if(nrow < 1){
    return ERROR_MULTICLASS_NO_TRAIN_DATA;
  }
  if(nrow < max_neighbors){
    return ERROR_MULTICLASS_TOO_MANY_NEIGHBORS;
  }
  if(max_neighbors < 1){
    return ERROR_MULTICLASS_TOO_FEW_NEIGHBORS;
  }
  // These two maps provide a convenient matrix/vector interface to
  // access the data at these pointers.
  Eigen::Map< Eigen::MatrixXd > train_inputs_mat(train_inputs_ptr, nrow, ncol);
  Eigen::Map< Eigen::VectorXd > test_input_vec(test_input_ptr, ncol);
  Eigen::Map< Eigen::VectorXd > distance_vec(distance_ptr, nrow);
  Eigen::Map< Eigen::VectorXi > sorted_index_vec(sorted_index_ptr, nrow);//to be sorted by dist.
  Eigen::Map< Eigen::VectorXi > label_count_vec(label_count_ptr, n_labels);
  for(int i=0; i<nrow; i++){
    distance_vec(i) = (train_inputs_mat.row(i).transpose()-test_input_vec).norm(); 
    sorted_index_vec(i) = i;//not sorted yet.
  }
  //std::cout << distance_vec.transpose() << std::endl << std::endl;
  std::sort
    (sorted_index_vec.data(),
     sorted_index_vec.data()+sorted_index_vec.size(),
     [&distance_vec](int lhs, int rhs){
      return distance_vec(lhs) < distance_vec(rhs);
    });
  //std::cout << sorted_index_vec.transpose() << std::endl << std::endl;
  label_count_vec.setZero();
  //std::cout << label_count_vec.transpose() << std::endl;
  int row, label;
  for(int model_i=0; model_i<max_neighbors; model_i++){
    row = sorted_index_vec(model_i);
    label = train_label_ptr[row];
    label_count_vec(label) += 1;
    //std::cout << label_count_vec.transpose() << std::endl;
    label_count_vec.maxCoeff(test_prediction_ptr+model_i);
  }
  return 0;
}

int Predict1toMaxNeighborsMatrixMultiClass
(double *train_inputs_ptr, //ntrain x ncol
 int *train_label_ptr,  //ntrain
 int n_train, int ncol, int max_neighbors, int n_test,
 int n_labels,
 double *test_inputs_ptr,     //ncol x ntest
 int *test_predictions_ptr //max_neighbors x ntest
 ){
  Eigen::Map< Eigen::MatrixXd > test_inputs_mat(test_inputs_ptr, ncol, n_test);
  Eigen::Map< Eigen::MatrixXi > test_predictions_mat
    (test_predictions_ptr, max_neighbors, n_test);
  if(n_train < 1){
    return ERROR_MULTICLASS_NO_TRAIN_DATA;
  }
  if(n_test < 1){
    return ERROR_MULTICLASS_NO_TEST_DATA;
  }
  if(n_train < max_neighbors){
    return ERROR_MULTICLASS_TOO_MANY_NEIGHBORS;
  }
  if(max_neighbors < 1){
    return ERROR_MULTICLASS_TOO_FEW_NEIGHBORS;
  }
#pragma omp parallel
  {
    Eigen::VectorXd distance_vec(n_train);
    Eigen::VectorXi sorted_index_vec(n_train), label_count_vec(n_labels);
    #pragma omp for
    for(int test_i=0; test_i<n_test; test_i++){
      //std::cout << test_i << " " << n_test << std::endl;
      Predict1toMaxNeighborsMultiClass
	(train_inputs_ptr, train_label_ptr,
	 n_train, ncol, max_neighbors, n_labels,
	 distance_vec.data(),
	 sorted_index_vec.data(),
	 label_count_vec.data(),
	 test_inputs_mat.col(test_i).data(),
	 test_predictions_mat.col(test_i).data()
	 );
      //std::cout << test_predictions_mat.transpose() << std::endl << std::endl;
    }
  }
  return 0;
}

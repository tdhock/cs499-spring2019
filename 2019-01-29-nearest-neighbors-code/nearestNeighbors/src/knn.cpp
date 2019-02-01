/* -*- compile-command: "R CMD INSTALL .. && R --vanilla < ../tests/testthat/test-knn.R" -*- */

#include "knn.h"
#include <Eigen/Dense>
#include <iostream>
#include <omp.h> 

int Predict1toMaxNeighbors
(double *train_inputs_ptr, double *train_label_ptr,
 int nrow, int ncol, int max_neighbors,
 double *distance_ptr, //nrow
 int *sorted_index_ptr, //nrow
 double *test_input_ptr,     // ncol
 double *test_prediction_ptr // max_neighbors
 ){
  if(nrow < 1){
    return ERROR_NO_TRAIN_DATA;
  }
  if(nrow < max_neighbors){
    return ERROR_TOO_MANY_NEIGHBORS;
  }
  if(max_neighbors < 1){
    return ERROR_TOO_FEW_NEIGHBORS;
  }
  // These two maps provide a convenient matrix/vector interface to
  // access the data at these pointers.
  Eigen::Map< Eigen::MatrixXd > train_inputs_mat(train_inputs_ptr, nrow, ncol);
  Eigen::Map< Eigen::VectorXd > test_input_vec(test_input_ptr, ncol);
  //std::cout << "Before distance computation" << std::endl; if we put
  //another pragma omp for here, it will be executed sequentially,
  //unless we tell it to do it in parallel.
  for(int i=0; i<nrow; i++){
    distance_ptr[i] = (train_inputs_mat.row(i).transpose()-test_input_vec).norm(); 
    //distance_vec(i) = (train_inputs_mat.row(i)-test_input_vec).norm();
    sorted_index_ptr[i] = i;//not sorted yet.
  }
  //std::cout << "After distance computation" << std::endl;
  std::sort
    (sorted_index_ptr,
     sorted_index_ptr+nrow,
     [&distance_vec](int lhs, int rhs){
      return distance_ptr[lhs] < distance_ptr[rhs];
    });
  //std::cout << sorted_index_vec << std::endl << std::endl;
  double total = 0.0;
  for(int model_i=0; model_i<max_neighbors; model_i++){
    int neighbors = model_i+1;
    int row = sorted_index_ptr[model_i];
    total += train_label_ptr[row];
    test_prediction_ptr[model_i] = total / neighbors;
  }
  return 0;
}

int Predict1toMaxNeighborsMatrix
(double *train_inputs_ptr, //ntrain x ncol
 double *train_label_ptr,  //ntrain
 int n_train, int ncol, int max_neighbors, int n_test,
 double *test_inputs_ptr,     //ncol x ntest
 double *test_predictions_ptr //max_neighbors x ntest
 ){
  Eigen::Map< Eigen::MatrixXd > test_inputs_mat(test_inputs_ptr, ncol, n_test);
  Eigen::Map< Eigen::MatrixXd > test_predictions_mat
    (test_predictions_ptr, max_neighbors, n_test);
  if(n_train < 1){
    return ERROR_NO_TRAIN_DATA;
  }
  if(n_test < 1){
    return ERROR_NO_TEST_DATA;
  }
  if(n_train < max_neighbors){
    return ERROR_TOO_MANY_NEIGHBORS;
  }
  if(max_neighbors < 1){
    return ERROR_TOO_FEW_NEIGHBORS;
  }
  //omp_set_num_threads(2);
  // dynamic overhead bad when each iteration is small. good when
  // there is a lot of variability between thread runtimes.
  //double t_start = omp_get_wtime();
  //O(n_test) barriers if we put this inside the sub-routine,
  //otherwise just one barrier as it is.
  //#pragma omp parallel for 
#pragma omp parallel
  {
    Eigen::VectorXd distance_vec(n_train);
    Eigen::VectorXi sorted_index_vec(n_train);//to be sorted by dist.
#pragma omp for
  for(int test_i=0; test_i<n_test; test_i++){
    Predict1toMaxNeighbors
      (train_inputs_ptr, train_label_ptr,
       n_train, ncol, max_neighbors,
       distance_vec.data(),
       sorted_index_vec.data(),
       test_inputs_mat.col(test_i).data(),
       test_predictions_mat.col(test_i).data()
       );
    //std::cout << test_predictions_mat.transpose() << std::endl << std::endl;
  }
  //double t_end = omp_get_wtime();
  //std::cout << "elapsed=" << t_end-t_start << std::endl;
  return 0;
}



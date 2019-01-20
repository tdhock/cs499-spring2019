/* -*- compile-command: "R CMD INSTALL .." -*- */

#include "knn.h"
#include <R.h>
#include <R_ext/Rdynload.h>

void Predict1toMaxNeighbors_interface
(double *train_inputs_ptr, double *train_label_ptr,
 int* nrow_ptr, int* ncol_ptr, int* max_neighbors_ptr,
 double *test_input_ptr,
 double *test_prediction_ptr
 ){
  int status = Predict1toMaxNeighbors
    (train_inputs_ptr, train_label_ptr,
     *nrow_ptr, *ncol_ptr, *max_neighbors_ptr,
     test_input_ptr,
     test_prediction_ptr);
  if(status == ERROR_TOO_MANY_NEIGHBORS){
    error("too many neighbors (should be at most nrow)");
  }
  if(status == ERROR_TOO_FEW_NEIGHBORS){
    error("too few neighbors (should be at least 1)");
  }
  if(status == ERROR_NO_TRAIN_DATA){
    error("no train data");
  }
  if(status != 0){
    error("unrecognized error ", status);
  }
}

void Predict1toMaxNeighborsMatrix_interface
(double *train_inputs_ptr, double *train_label_ptr,
 int* n_train_ptr, int* ncol_ptr, int* max_neighbors_ptr, int* n_test_ptr,
 double *test_input_ptr,
 double *test_prediction_ptr
 ){
  int status = Predict1toMaxNeighborsMatrix
    (train_inputs_ptr, train_label_ptr,
     *n_train_ptr, *ncol_ptr, *max_neighbors_ptr, *n_test_ptr,
     test_input_ptr,
     test_prediction_ptr);
  if(status == ERROR_TOO_MANY_NEIGHBORS){
    error("too many neighbors (should be at most nrow)");
  }
  if(status == ERROR_TOO_FEW_NEIGHBORS){
    error("too few neighbors (should be at least 1)");
  }
  if(status == ERROR_NO_TRAIN_DATA){
    error("no train data");
  }
  if(status == ERROR_NO_TEST_DATA){
    error("no test data");
  }
  if(status != 0){
    error("unrecognized error ", status);
  }
}

R_CMethodDef cMethods[] = {
  {"Predict1toMaxNeighbors_interface", (DL_FUNC) &Predict1toMaxNeighbors_interface, 7},
  {"Predict1toMaxNeighborsMatrix_interface", (DL_FUNC) &Predict1toMaxNeighborsMatrix_interface, 8},
  {NULL, NULL, 0}
};

extern "C" {
  void R_init_nearestNeighbors(DllInfo *info) {
    R_registerRoutines(info, cMethods, NULL, NULL, NULL);
    //R_useDynamicSymbols call says the DLL is not to be searched for
    //entry points specified by character strings so .C etc calls will
    //only find registered symbols.
    R_useDynamicSymbols(info, FALSE);
  }
}

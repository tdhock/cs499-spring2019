int Predict1toMaxNeighborsMultiClass
(double *train_inputs_ptr, int *train_label_ptr,
 int nrow, int ncol, int max_neighbors,  int,
 double*,
 int*,
 int*,
 double *test_input_ptr,     // ncol
 int *test_prediction_ptr // max_neighbors
 );

int Predict1toMaxNeighborsMatrixMultiClass
(double *train_inputs_ptr, //ntrain x ncol
 int *train_label_ptr,  //ntrain
 int n_train, int ncol, int max_neighbors, int n_test,
 int n_labels,
 double *test_inputs_ptr,     //ncol x ntest
 int *test_predictions_ptr //max_neighbors x ntest
 );

#define ERROR_MULTICLASS_TOO_MANY_NEIGHBORS 1
#define ERROR_MULTICLASS_TOO_FEW_NEIGHBORS 2
#define ERROR_MULTICLASS_NO_TRAIN_DATA 3
#define ERROR_MULTICLASS_NO_TEST_DATA 4



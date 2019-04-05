int Predict1toMaxNeighbors
(double *train_inputs_ptr, double *train_label_ptr,
 int nrow, int ncol, int max_neighbors,
 double *test_input_ptr, double*,
 double*, int*,
 double *test_prediction_ptr
 );

int Predict1toMaxNeighborsMatrix
(double *train_inputs_ptr, double *train_label_ptr,
 int nrow, int ncol, int max_neighbors, int ntest,
 double *test_input_ptr, double*,
 double *test_prediction_ptr
 );

#define ERROR_TOO_MANY_NEIGHBORS 1
#define ERROR_TOO_FEW_NEIGHBORS 2
#define ERROR_NO_TRAIN_DATA 3
#define ERROR_NO_TEST_DATA 4


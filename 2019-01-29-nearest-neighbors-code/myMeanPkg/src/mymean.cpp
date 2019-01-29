#include "mymean.h"

int my_mean_C(int *data_ptr, int data_count, double *output_ptr){
  if(data_count==0){
    return MY_MEAN_ERROR_NO_DATA;
  }
  double total = 0.0;
  for(int data_i=0; data_i<data_count; data_i++){
    total += data_ptr[data_i];
  }
  *output_ptr = total/data_count;
  return 0;
}

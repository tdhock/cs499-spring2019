/* -*- compile-command: "R CMD INSTALL .." -*- */

#include "mymean.h"
#include "eigen_mean.h"
#include <R.h>
#include <R_ext/Rdynload.h>

void my_mean_interface(int *data_ptr, int *data_count, double *output_ptr){
  int status = my_mean_C(data_ptr, *data_count, output_ptr);
  if(status == MY_MEAN_ERROR_NO_DATA){
    error("no data");
  }
  if(status != 0){
    error("unrecognized error ", status);
  }
}
void eigen_mean_interface(double *data_ptr, int *data_count, double *output_ptr){
  int status = eigen_mean_Cpp(data_ptr, *data_count, output_ptr);
  if(status == EIGEN_MEAN_ERROR_NO_DATA){
    error("no data");
  }
  if(status != 0){
    error("unrecognized error ", status);
  }
}

R_CMethodDef cMethods[] = {
  {"my_mean_interface", (DL_FUNC) &my_mean_interface, 3},
  {"eigen_mean_interface", (DL_FUNC) &eigen_mean_interface, 3},
  {NULL, NULL, 0}
};

extern "C" {
  void R_init_myMeanPkg(DllInfo *info) {
    R_registerRoutines(info, cMethods, NULL, NULL, NULL);
    //R_useDynamicSymbols call says the DLL is not to be searched for
    //entry points specified by character strings so .C etc calls will
    //only find registered symbols.
    R_useDynamicSymbols(info, FALSE);
  }
}

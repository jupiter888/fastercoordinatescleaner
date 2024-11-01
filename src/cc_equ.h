#ifndef CC_EQU_H
#define CC_EQU_H

#include <Rcpp.h>
using namespace Rcpp;

LogicalVector cc_equ_cpp(NumericVector lon, NumericVector lat, std::string test);

#endif  // CC_EQU_H

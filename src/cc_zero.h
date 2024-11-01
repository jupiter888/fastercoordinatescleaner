#ifndef CC_ZERO_H
#define CC_ZERO_H

#include <Rcpp.h>
using namespace Rcpp;

LogicalVector cc_zero_cpp(NumericVector lon, NumericVector lat, double buffer);

#endif  // CC_ZERO_H

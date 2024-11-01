#ifndef CC_CAP_H
#define CC_CAP_H

#include <Rcpp.h>
using namespace Rcpp;

LogicalVector cc_cap_cpp(NumericMatrix points, double buffer, bool geod, NumericMatrix ref_coords);

#endif  // CC_CAP_H

#ifndef CC_SEA_H
#define CC_SEA_H

#include <Rcpp.h>
using namespace Rcpp;

LogicalVector cc_sea_cpp(NumericMatrix coords, List land_polygons, double buffer = 0);

#endif  // CC_SEA_H

#ifndef CC_URB_H
#define CC_URB_H

#include <Rcpp.h>
using namespace Rcpp;

LogicalVector cc_urb_cpp(NumericMatrix coords, List urban_polygons, double buffer = 0);

#endif  // CC_URB_H

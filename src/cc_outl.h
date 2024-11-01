#ifndef CC_OUTL_H
#define CC_OUTL_H

#include <Rcpp.h>
using namespace Rcpp;

LogicalVector cc_outl_cpp(NumericVector longitudes, NumericVector latitudes,
                          std::string method = "quantile", double mltpl = 1.5,
                          double tdi = 1000, int min_occs = 7, bool intrinsic = false);

#endif  // CC_OUTL_H

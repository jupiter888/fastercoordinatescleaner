#ifndef CC_DUPL_H
#define CC_DUPL_H

#include <Rcpp.h>
using namespace Rcpp;

LogicalVector cc_dupl_cpp(NumericVector lon, NumericVector lat, CharacterVector species, List additions);

#endif  // CC_DUPL_H

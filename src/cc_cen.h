#ifndef CC_CEN_H
#define CC_CEN_H

#include <Rcpp.h>
using namespace Rcpp;

LogicalVector cc_cen_cpp(DataFrame x, std::string lon, std::string lat, std::string species,
                         double buffer, bool geod, std::string test, DataFrame ref,
                         bool verify, std::string value, bool verbose);

#endif  // CC_CEN_H

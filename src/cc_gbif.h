#ifndef CC_GBIF_H
#define CC_GBIF_H

#include <Rcpp.h>
using namespace Rcpp;

Rcpp::LogicalVector cc_gbif_cpp(Rcpp::DataFrame x, std::string lon_col = "decimalLongitude",
                                std::string lat_col = "decimalLatitude", double lon_ref = 0.0,
                                double lat_ref = 0.0, double max_dist = 100000);

#endif  // CC_GBIF_H

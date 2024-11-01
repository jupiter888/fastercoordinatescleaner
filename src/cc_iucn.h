#ifndef CC_IUCN_H
#define CC_IUCN_H

#include <Rcpp.h>
using namespace Rcpp;

Rcpp::List cc_iucn_cpp(Rcpp::DataFrame x, Rcpp::List ranges, std::string lon_col = "decimalLongitude",
                       std::string lat_col = "decimalLatitude", std::string species_col = "species",
                       double buffer = 0, std::string value = "clean", bool verbose = true);

#endif  // CC_IUCN_H

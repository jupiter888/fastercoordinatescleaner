#ifndef CC_INST_H
#define CC_INST_H

#include <Rcpp.h>
using namespace Rcpp;

Rcpp::List cc_inst_cpp(Rcpp::DataFrame x, Rcpp::NumericVector inst_lon, Rcpp::NumericVector inst_lat,
                       std::string lon_col = "decimalLongitude", std::string lat_col = "decimalLatitude",
                       std::string species_col = "species", double buffer = 100, bool geod = false,
                       bool verify = false, double verify_mltpl = 10, std::string value = "clean",
                       bool verbose = true);

#endif  // CC_INST_H

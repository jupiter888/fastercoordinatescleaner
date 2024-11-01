#ifndef CC_COUN_H
#define CC_COUN_H

#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;

Rcpp::DataFrame cc_coun_cpp(Rcpp::DataFrame x, std::string lon_col = "decimalLongitude",
                            std::string lat_col = "decimalLatitude", std::string iso3_col = "countryCode",
                            Rcpp::Nullable<Rcpp::NumericVector> country_lon_centroids = R_NilValue,
                            Rcpp::Nullable<Rcpp::NumericVector> country_lat_centroids = R_NilValue,
                            Rcpp::Nullable<Rcpp::StringVector> country_iso3_codes = R_NilValue,
                            double buffer = 0.0, std::string value = "clean", bool verbose = true);

#endif  // CC_COUN_H

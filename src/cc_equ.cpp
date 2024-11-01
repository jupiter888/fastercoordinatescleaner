#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector cc_equ_cpp(NumericVector lon, NumericVector lat, std::string test) {
  int n = lon.size();
  LogicalVector result(n, true);

  if (test == "absolute") {
    for (int i = 0; i < n; ++i) {
      if (std::abs(lon[i]) == std::abs(lat[i])) {
        result[i] = false;
      }
    }
  } else if (test == "identical") {
    for (int i = 0; i < n; ++i) {
      if (lon[i] == lat[i]) {
        result[i] = false;
      }
    }
  }

  return result;
}

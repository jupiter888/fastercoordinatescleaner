#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector cc_val_cpp(NumericVector lon, NumericVector lat) {
  int n = lon.size();
  LogicalVector result(n, true);

  for (int i = 0; i < n; ++i) {
    if (NumericVector::is_na(lon[i]) || NumericVector::is_na(lat[i]) ||
        lon[i] < -180 || lon[i] > 180 || lat[i] < -90 || lat[i] > 90) {
      result[i] = false;
    }
  }

  return result;
}

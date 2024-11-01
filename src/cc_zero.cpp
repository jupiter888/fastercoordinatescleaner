#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector cc_zero_cpp(NumericVector lon, NumericVector lat, double buffer) {
  int n = lon.size();
  LogicalVector result(n, true);
  double buffer_squared = buffer * buffer;

  for (int i = 0; i < n; ++i) {
    if (lon[i] == 0 || lat[i] == 0 || (lon[i] * lon[i] + lat[i] * lat[i] <= buffer_squared)) {
      result[i] = false;
    }
  }

  return result;
}

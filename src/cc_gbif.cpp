#include <Rcpp.h>
#include <cmath>

// Inline Haversine formula calculation function
inline double haversine(double lon1, double lat1, double lon2, double lat2) {
  const double R = 6371000; // Radius of Earth in meters
  const double to_rad = M_PI / 180.0;

  double d_lat = (lat2 - lat1) * to_rad;
  double d_lon = (lon2 - lon1) * to_rad;

  lat1 *= to_rad;
  lat2 *= to_rad;

  double a = std::sin(d_lat / 2) * std::sin(d_lat / 2) +
    std::sin(d_lon / 2) * std::sin(d_lon / 2) * std::cos(lat1) * std::cos(lat2);
  double c = 2 * std::atan2(std::sqrt(a), std::sqrt(1 - a));

  return R * c;
}

// Example GBIF record validation function
// [[Rcpp::export]]
Rcpp::LogicalVector cc_gbif_cpp(Rcpp::DataFrame x,
                                std::string lon_col = "decimalLongitude",
                                std::string lat_col = "decimalLatitude",
                                double lon_ref = 0.0,
                                double lat_ref = 0.0,
                                double max_dist = 100000) {
  Rcpp::NumericVector lon = x[lon_col];
  Rcpp::NumericVector lat = x[lat_col];
  int n = lon.size();
  Rcpp::LogicalVector is_valid(n);

  for (int i = 0; i < n; i++) {
    double distance = haversine(lon[i], lat[i], lon_ref, lat_ref);
    is_valid[i] = (distance <= max_dist);
  }

  return is_valid;
}

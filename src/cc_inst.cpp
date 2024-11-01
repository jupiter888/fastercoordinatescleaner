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

// [[Rcpp::export]]
Rcpp::List cc_inst_cpp(Rcpp::DataFrame x,
                       Rcpp::NumericVector inst_lon,
                       Rcpp::NumericVector inst_lat,
                       std::string lon_col = "decimalLongitude",
                       std::string lat_col = "decimalLatitude",
                       std::string species_col = "species",
                       double buffer = 100,
                       bool geod = false,
                       bool verify = false,
                       double verify_mltpl = 10,
                       std::string value = "clean",
                       bool verbose = true) {

  Rcpp::NumericVector lon = x[lon_col];
  Rcpp::NumericVector lat = x[lat_col];
  Rcpp::StringVector species = x[species_col];

  int n = lon.size();
  Rcpp::LogicalVector is_clean(n, true);  // Default to TRUE (clean)

  // Convert buffer from meters to degrees if not using geodetic distance
  if (!geod) {
    buffer = buffer / 111000.0;  // Approximate conversion (1 degree ~ 111 km)
  }

  for (int i = 0; i < n; i++) {
    bool flag = false;
    for (int j = 0; j < inst_lon.size(); j++) {
      double distance = (geod)
      ? haversine(lon[i], lat[i], inst_lon[j], inst_lat[j])
        : std::sqrt(std::pow(lon[i] - inst_lon[j], 2) + std::pow(lat[i] - inst_lat[j], 2)) * 111000;

      if (distance <= buffer) {
        flag = true;
        break;
      }
    }
    is_clean[i] = !flag;  // Flagged as near an institution
  }

  // Verification step if enabled
  if (verify) {
    for (int i = 0; i < n; i++) {
      if (!is_clean[i]) {
        bool other_species_nearby = false;
        for (int j = 0; j < n; j++) {
          if (i != j && species[i] == species[j]) {
            double distance = haversine(lon[i], lat[i], lon[j], lat[j]);
            if (distance <= buffer * verify_mltpl) {
              other_species_nearby = true;
              break;
            }
          }
        }
        if (other_species_nearby) {
          is_clean[i] = true;  // Unflag if other records are nearby
        }
      }
    }
  }

  if (value == "clean") {
    Rcpp::DataFrame clean_data = x[is_clean];
    return Rcpp::List::create(Rcpp::Named("data") = clean_data);
  } else {
    return Rcpp::List::create(Rcpp::Named("flags") = is_clean);
  }
}

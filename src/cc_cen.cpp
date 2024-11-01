#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector cc_cen_cpp(DataFrame x,
                         std::string lon,
                         std::string lat,
                         std::string species,
                         double buffer,
                         bool geod,
                         std::string test,
                         DataFrame ref,   // Remove the default argument here
                         bool verify,
                         std::string value,
                         bool verbose) {

  int n = x.nrows();
  LogicalVector out(n, true);

  // Check value and test arguments
  if (value != "clean" && value != "flagged") {
    stop("Invalid value argument");
  }
  if (test != "both" && test != "country" && test != "provinces") {
    stop("Invalid test argument");
  }

  // Handle verbose messaging
  if (verbose) {
    Rcpp::Rcout << "Testing country centroids" << std::endl;
  }
  if (buffer > 10 && !geod) {
    warning("Using large buffer, check 'geod'");
  }
  if (buffer < 100 && geod) {
    warning("Using small buffer, check 'geod'");
  }

  // Extract coordinates from x
  NumericVector x_lon = x[lon];
  NumericVector x_lat = x[lat];

  // Extract coordinates from ref
  NumericVector ref_lon = ref["centroid.lon"];
  NumericVector ref_lat = ref["centroid.lat"];

  // Calculate distances and flag problematic records
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < ref.nrows(); ++j) {
      double d_lon = x_lon[i] - ref_lon[j];
      double d_lat = x_lat[i] - ref_lat[j];

      // Simple Euclidean distance, needs adjustment for spherical calculations
      double distance = sqrt(d_lon * d_lon + d_lat * d_lat);

      if (geod) {
        // Convert degrees to meters (approximate)
        distance *= 111320.0;
      }

      if (distance <= buffer) {
        out[i] = false;
        break;
      }
    }
  }

  // Verification step
  if (verify) {
    NumericVector flag_count(n, 0.0);
    for (int i = 0; i < n; ++i) {
      if (!out[i]) {
        for (int j = 0; j < n; ++j) {
          if (x_lon[i] == x_lon[j] && x_lat[i] == x_lat[j]) {
            flag_count[i]++;
          }
        }
        if (flag_count[i] > 1) {
          out[i] = true;
        }
      }
    }
  }

  // Return results
  if (value == "clean") {
    DataFrame result = x[out];
    return wrap(result);
  } else {
    return out;
  }
}

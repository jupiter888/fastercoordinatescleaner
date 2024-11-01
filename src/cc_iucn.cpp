#include <Rcpp.h>
#include <cmath>

// Function to calculate whether a point is within a bounding box
bool point_in_bbox(double lon, double lat,
                   double min_lon, double min_lat,
                   double max_lon, double max_lat) {
  return (lon >= min_lon && lon <= max_lon && lat >= min_lat && lat <= max_lat);
}

// [[Rcpp::export]]
Rcpp::List cc_iucn_cpp(Rcpp::DataFrame x,
                       Rcpp::List ranges, // List of ranges (each as a named list with species and bbox)
                       std::string lon_col = "decimalLongitude",
                       std::string lat_col = "decimalLatitude",
                       std::string species_col = "species",
                       double buffer = 0,
                       std::string value = "clean",
                       bool verbose = true) {

  Rcpp::NumericVector lon = x[lon_col];
  Rcpp::NumericVector lat = x[lat_col];
  Rcpp::StringVector species = x[species_col];

  int n = lon.size();
  Rcpp::LogicalVector is_clean(n, true);

  if (verbose) {
    Rcpp::Rcout << "Testing natural ranges for species..." << std::endl;
  }

  for (int i = 0; i < n; i++) {
    std::string current_species = Rcpp::as<std::string>(species[i]);
    bool found = false;

    for (int j = 0; j < ranges.size(); j++) {
      Rcpp::List range_data = ranges[j];
      std::string range_species = Rcpp::as<std::string>(range_data["species"]);

      if (range_species == current_species) {
        double min_lon = Rcpp::as<double>(range_data["min_lon"]);
        double min_lat = Rcpp::as<double>(range_data["min_lat"]);
        double max_lon = Rcpp::as<double>(range_data["max_lon"]);
        double max_lat = Rcpp::as<double>(range_data["max_lat"]);

        if (buffer != 0) {
          min_lon -= buffer / 111000.0;  // Approximate conversion from meters to degrees
          max_lon += buffer / 111000.0;
          min_lat -= buffer / 111000.0;
          max_lat += buffer / 111000.0;
        }

        if (point_in_bbox(lon[i], lat[i], min_lon, min_lat, max_lon, max_lat)) {
          found = true;
          break;
        }
      }
    }

    if (!found) {
      is_clean[i] = false;  // Mark as outside range
    }
  }

  if (verbose) {
    int flagged = std::count(is_clean.begin(), is_clean.end(), false);
    if (value == "clean") {
      Rcpp::Rcout << "Removed " << flagged << " records." << std::endl;
    } else {
      Rcpp::Rcout << "Flagged " << flagged << " records." << std::endl;
    }
  }

  if (value == "clean") {
    Rcpp::DataFrame clean_data = x[is_clean];
    return Rcpp::List::create(Rcpp::Named("data") = clean_data);
  } else {
    return Rcpp::List::create(Rcpp::Named("flags") = is_clean);
  }
}

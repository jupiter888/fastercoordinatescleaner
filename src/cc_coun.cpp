#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// Inline function to calculate the Haversine distance between two geographic points
inline double haversine_distance(double lat1, double lon1, double lat2, double lon2) {
  const double EARTH_RADIUS = 6371000.0;  // Earth radius in meters
  const double DEG_TO_RAD = M_PI / 180.0;

  // Convert degrees to radians
  double phi1 = lat1 * DEG_TO_RAD;
  double phi2 = lat2 * DEG_TO_RAD;
  double delta_phi = (lat2 - lat1) * DEG_TO_RAD;
  double delta_lambda = (lon2 - lon1) * DEG_TO_RAD;

  // Haversine formula
  double a = sin(delta_phi / 2.0) * sin(delta_phi / 2.0) +
    cos(phi1) * cos(phi2) * sin(delta_lambda / 2.0) * sin(delta_lambda / 2.0);
  double c = 2 * atan2(sqrt(a), sqrt(1 - a));

  return EARTH_RADIUS * c;  // Distance in meters
}

// [[Rcpp::export]]
Rcpp::DataFrame cc_coun_cpp(Rcpp::DataFrame x,
                            std::string lon_col = "decimalLongitude",
                            std::string lat_col = "decimalLatitude",
                            std::string iso3_col = "countryCode",
                            Rcpp::Nullable<Rcpp::NumericVector> country_lon_centroids = R_NilValue,
                            Rcpp::Nullable<Rcpp::NumericVector> country_lat_centroids = R_NilValue,
                            Rcpp::Nullable<Rcpp::StringVector> country_iso3_codes = R_NilValue,
                            double buffer = 0.0,
                            std::string value = "clean",
                            bool verbose = true) {

  // Extract longitude, latitude, and ISO3 columns from the input DataFrame
  Rcpp::NumericVector lon = x[lon_col];
  Rcpp::NumericVector lat = x[lat_col];
  Rcpp::StringVector iso3 = x[iso3_col];

  // Initialize vectors for centroids and iso3 codes
  Rcpp::NumericVector lon_centroids, lat_centroids;
  Rcpp::StringVector iso3_codes;

  // Handle Nullable arguments and convert them into NumericVector if provided
  if (country_lon_centroids.isNotNull()) {
    lon_centroids = Rcpp::as<Rcpp::NumericVector>(country_lon_centroids);
  } else {
    stop("Country longitude centroids not provided.");
  }

  if (country_lat_centroids.isNotNull()) {
    lat_centroids = Rcpp::as<Rcpp::NumericVector>(country_lat_centroids);
  } else {
    stop("Country latitude centroids not provided.");
  }

  if (country_iso3_codes.isNotNull()) {
    iso3_codes = Rcpp::as<Rcpp::StringVector>(country_iso3_codes);
  } else {
    stop("Country ISO3 codes not provided.");
  }

  int n = lon.size();  // Number of records in the input DataFrame
  int country_count = iso3_codes.size();

  Rcpp::LogicalVector within_country(n, false);  // Vector to store whether each record is within the correct country

  // Iterate over each record in the data
  for (int i = 0; i < n; i++) {
    std::string record_iso3 = Rcpp::as<std::string>(iso3[i]);
    bool matched = false;

    // Check each country polygon's centroid
    for (int j = 0; j < country_count; j++) {
      std::string country_iso3 = Rcpp::as<std::string>(iso3_codes[j]);

      if (record_iso3 == country_iso3) {
        // Calculate the Haversine distance between the record's coordinates and the country's centroid
        double distance = haversine_distance(lat[i], lon[i], lat_centroids[j], lon_centroids[j]);

        // If a buffer is defined, check if the distance is within the buffer zone
        if (distance <= buffer) {
          within_country[i] = true;
          matched = true;
          break;  // No need to check other countries once matched
        }
      }
    }

    // If no match is found, the record is considered outside the country
    if (!matched) {
      within_country[i] = false;
    }
  }

  // Verbose output
  if (verbose) {
    int flagged = std::count(within_country.begin(), within_country.end(), false);
    Rcpp::Rcout << "Flagged " << flagged << " records." << std::endl;
  }

  // Return the cleaned data or flagged data depending on the 'value' parameter
  if (value == "clean") {
    return x[within_country];
  } else {
    return Rcpp::DataFrame::create(Rcpp::Named("flagged") = within_country);
  }
}

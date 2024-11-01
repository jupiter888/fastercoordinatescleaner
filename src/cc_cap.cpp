#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]

const double EARTH_RADIUS = 6371000.0;  // Earth radius in meters
const double DEG_TO_RAD = 3.14159265358979323846 / 180.0;  // Precomputed for faster radian conversion

// Convert degrees to radians
inline double deg2rad(double deg) {
  return deg * DEG_TO_RAD;
}

// Calculate geodesic distance between two points
inline double geodesic_distance(double lon1, double lat1, double lon2, double lat2) {
  double phi1 = deg2rad(lat1);
  double phi2 = deg2rad(lat2);
  double delta_phi = phi2 - phi1;
  double delta_lambda = deg2rad(lon2 - lon1);

  double a = sin(delta_phi / 2.0) * sin(delta_phi / 2.0) +
    cos(phi1) * cos(phi2) * sin(delta_lambda / 2.0) * sin(delta_lambda / 2.0);
  return 2.0 * EARTH_RADIUS * atan2(sqrt(a), sqrt(1.0 - a));
}

// Calculate planar distance between two points
inline double planar_distance(double lon1, double lat1, double lon2, double lat2) {
  double x = (lon2 - lon1) * cos((lat1 + lat2) * DEG_TO_RAD / 2.0);
  double y = lat2 - lat1;
  return 111319.9 * sqrt(x * x + y * y);  // Convert degrees to meters
}

//' @title Check coordinates against capital cities
 //' @param points NumericMatrix with longitude and latitude columns
 //' @param buffer numeric buffer distance in meters
 //' @param geod logical indicating whether to use geodesic distance
 //' @param ref_coords NumericMatrix with reference coordinates
 //' @return LogicalVector indicating which records are valid (not within buffer of capitals)
 // [[Rcpp::export]]
 LogicalVector cc_cap_cpp(NumericMatrix points,
                          double buffer,
                          bool geod,
                          NumericMatrix ref_coords) {

   int n_points = points.nrow();
   int n_refs = ref_coords.nrow();
   LogicalVector result(n_points, true);

   for (int i = 0; i < n_points; i++) {
     double point_lon = points(i, 0);
     double point_lat = points(i, 1);

     for (int j = 0; j < n_refs; j++) {
       double ref_lon = ref_coords(j, 0);
       double ref_lat = ref_coords(j, 1);

       // Choose distance function based on 'geod'
       double dist = geod ? geodesic_distance(point_lon, point_lat, ref_lon, ref_lat)
         : planar_distance(point_lon, point_lat, ref_lon, ref_lat);

       if (dist <= buffer) {
         result[i] = false;
         break;  // Early exit if within buffer
       }
     }
   }

   return result;
 }

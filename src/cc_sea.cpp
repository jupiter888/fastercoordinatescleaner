#include <Rcpp.h>
using namespace Rcpp;

// Static function to check if a point is inside a polygon using ray-casting
static bool isPointInPolygon(double x, double y, NumericMatrix polygon) {
  int nvert = polygon.nrow();
  bool inside = false;

  for (int i = 0, j = nvert - 1; i < nvert; j = i++) {
    double xi = polygon(i, 0), yi = polygon(i, 1);
    double xj = polygon(j, 0), yj = polygon(j, 1);

    if (((yi > y) != (yj > y)) &&
        (x < (xj - xi) * (y - yi) / (yj - yi) + xi)) {
      inside = !inside;
    }
  }
  return inside;
}

// [[Rcpp::export]]
LogicalVector cc_sea_cpp(NumericMatrix coords, List land_polygons, double buffer = 0) {
  int n_points = coords.nrow();
  LogicalVector result(n_points);

  // Iterate over all points
  for (int i = 0; i < n_points; i++) {
    double lon = coords(i, 0);
    double lat = coords(i, 1);
    bool is_land = false;

    // Check if the point is inside any polygon
    for (int j = 0; j < land_polygons.size(); j++) {
      NumericMatrix polygon = land_polygons[j];

      if (isPointInPolygon(lon, lat, polygon)) {
        is_land = true;
        break;
      }
    }
    result[i] = !is_land; // Invert to flag sea points
  }

  return result;
}

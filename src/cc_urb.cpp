#include <Rcpp.h>
using namespace Rcpp;

// Static function for point-in-polygon test using ray-casting
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
LogicalVector cc_urb_cpp(NumericMatrix coords, List urban_polygons,
                         double buffer = 0) {
  int n_points = coords.nrow();
  LogicalVector result(n_points);

  // Iterate over all points
  for (int i = 0; i < n_points; i++) {
    double lon = coords(i, 0);
    double lat = coords(i, 1);
    bool is_urban = false;

    // Check if the point is inside any urban polygon
    for (int j = 0; j < urban_polygons.size(); j++) {
      NumericMatrix polygon = urban_polygons[j];

      // Apply buffer if necessary (currently omitted for simplicity)
      if (isPointInPolygon(lon, lat, polygon)) {
        is_urban = true;
        break;
      }
    }
    result[i] = is_urban;
  }

  return result;
}

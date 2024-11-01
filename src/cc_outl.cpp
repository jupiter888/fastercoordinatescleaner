#include <Rcpp.h>
#include <algorithm>
#include <cmath>

using namespace Rcpp;

// Inline function to calculate the median
double inline_median(NumericVector x) {
  NumericVector sorted_x = clone(x).sort();
  int n = sorted_x.size();
  if (n % 2 == 0) {
    return (sorted_x[n / 2 - 1] + sorted_x[n / 2]) / 2.0;
  } else {
    return sorted_x[n / 2];
  }
}

// Inline function to calculate the MAD (Median Absolute Deviation)
double inline_mad(NumericVector x) {
  double med = inline_median(x);
  NumericVector abs_devs = abs(x - med);
  return inline_median(abs_devs);
}

// Inline function to compute Euclidean distance
double inline_euclidean_distance(double lon1, double lat1, double lon2, double lat2) {
  double dx = lon2 - lon1;
  double dy = lat2 - lat1;
  return std::sqrt(dx * dx + dy * dy);
}

// [[Rcpp::export]]
LogicalVector cc_outl_cpp(NumericVector longitudes, NumericVector latitudes,
                          std::string method = "quantile", double mltpl = 1.5,
                          double tdi = 1000, int min_occs = 7, bool intrinsic = false) {

  int n = longitudes.size();
  LogicalVector outliers(n, false);
  NumericMatrix dist_geo(n, n);

  if (intrinsic) {
    for (int i = 0; i < n; ++i) {
      for (int j = i + 1; j < n; ++j) {
        dist_geo(i, j) = inline_euclidean_distance(longitudes[i], latitudes[i], longitudes[j], latitudes[j]);
        dist_geo(j, i) = dist_geo(i, j);
      }
    }
  }

  NumericVector mean_distances(n);

  for (int i = 0; i < n; ++i) {
    double total_dist = 0.0;
    int count = 0;
    for (int j = 0; j < n; ++j) {
      if (i != j) {
        total_dist += dist_geo(i, j);
        count++;
      }
    }
    mean_distances[i] = total_dist / count;
  }

  if (method == "quantile") {
    NumericVector sorted_mean = clone(mean_distances).sort();
    double q75 = sorted_mean[n * 3 / 4];
    double iqr = q75 - sorted_mean[n / 4];
    double threshold = q75 + mltpl * iqr;

    for (int i = 0; i < n; ++i) {
      if (mean_distances[i] > threshold) {
        outliers[i] = true;
      }
    }
  } else if (method == "mad") {
    double median_val = inline_median(mean_distances);
    double mad_val = inline_mad(mean_distances);
    double threshold = median_val + mltpl * mad_val;

    for (int i = 0; i < n; ++i) {
      if (mean_distances[i] > threshold) {
        outliers[i] = true;
      }
    }
  } else if (method == "distance") {
    for (int i = 0; i < n; ++i) {
      double min_dist = R_PosInf;
      for (int j = 0; j < n; ++j) {
        if (i != j && dist_geo(i, j) < min_dist) {
          min_dist = dist_geo(i, j);
        }
      }
      if (min_dist > tdi) {
        outliers[i] = true;
      }
    }
  }

  return outliers;
}

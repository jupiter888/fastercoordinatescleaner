#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <unordered_map>

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
LogicalVector cc_outl_cpp(DataFrame df,
                          std::string lon_col, std::string lat_col, std::string species_col,
                          std::string method = "quantile", double mltpl = 1.5,
                          double tdi = 1000, int min_occs = 7, bool intrinsic = false) {
  NumericVector longitudes = df[lon_col];
  NumericVector latitudes = df[lat_col];
  CharacterVector species = df[species_col];

  int n = longitudes.size();
  LogicalVector outliers(n, false);

  // Group indices by species
  std::unordered_map<std::string, std::vector<int>> species_map;
  for (int i = 0; i < n; ++i) {
    species_map[as<std::string>(species[i])].push_back(i);
  }

  // Iterate through each species
  for (const auto& entry : species_map) {
    const std::vector<int>& indices = entry.second;
    int species_size = indices.size();

    if (species_size < min_occs) {
      continue;
    }

    NumericVector mean_distances(species_size);
    NumericMatrix dist_geo(species_size, species_size);

    // Compute distance matrix if intrinsic is true
    if (intrinsic) {
      for (int i = 0; i < species_size; ++i) {
        for (int j = i + 1; j < species_size; ++j) {
          double dist = inline_euclidean_distance(longitudes[indices[i]], latitudes[indices[i]],
                                                  longitudes[indices[j]], latitudes[indices[j]]);
          dist_geo(i, j) = dist;
          dist_geo(j, i) = dist;
        }
      }
    }

    // Calculate mean distances
    for (int i = 0; i < species_size; ++i) {
      double total_dist = 0.0;
      int count = 0;
      for (int j = 0; j < species_size; ++j) {
        if (i != j) {
          if (intrinsic) {
            total_dist += dist_geo(i, j);
          } else {
            total_dist += inline_euclidean_distance(longitudes[indices[i]], latitudes[indices[i]],
                                                    longitudes[indices[j]], latitudes[indices[j]]);
          }
          count++;
        }
      }
      mean_distances[i] = total_dist / count;
    }

    // Determine outliers based on the chosen method
    if (method == "quantile") {
      NumericVector sorted_mean = clone(mean_distances).sort();
      double q75 = sorted_mean[species_size * 3 / 4];
      double iqr = q75 - sorted_mean[species_size / 4];
      double threshold = q75 + mltpl * iqr;

      for (int i = 0; i < species_size; ++i) {
        if (mean_distances[i] > threshold) {
          outliers[indices[i]] = true;
        }
      }
    } else if (method == "mad") {
      double median_val = inline_median(mean_distances);
      double mad_val = inline_mad(mean_distances);
      double threshold = median_val + mltpl * mad_val;

      for (int i = 0; i < species_size; ++i) {
        if (mean_distances[i] > threshold) {
          outliers[indices[i]] = true;
        }
      }
    } else if (method == "distance") {
      for (int i = 0; i < species_size; ++i) {
        double min_dist = R_PosInf;
        for (int j = 0; j < species_size; ++j) {
          if (i != j) {
            double dist = (intrinsic) ? dist_geo(i, j) : inline_euclidean_distance(longitudes[indices[i]], latitudes[indices[i]],
                           longitudes[indices[j]], latitudes[indices[j]]);
            if (dist < min_dist) {
              min_dist = dist;
            }
          }
        }
        if (min_dist > tdi) {
          outliers[indices[i]] = true;
        }
      }
    }
  }

  return outliers;
}

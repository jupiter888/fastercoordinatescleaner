#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector cc_dupl_cpp(NumericVector lon, NumericVector lat, CharacterVector species, List additions) {
  int n = lon.size();
  LogicalVector result(n, true);

  std::unordered_map<std::string, int> record_map;

  for (int i = 0; i < n; ++i) {
    std::ostringstream record;
    record << lon[i] << "," << lat[i] << "," << as<std::string>(species[i]);

    for (int j = 0; j < additions.size(); ++j) {
      CharacterVector addition = additions[j];
      record << "," << as<std::string>(addition[i]);
    }

    if (record_map.find(record.str()) != record_map.end()) {
      result[i] = false;
    } else {
      record_map[record.str()] = 1;
    }
  }

  return result;
}

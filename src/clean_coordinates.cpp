// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

// include necessary headers for the "cc_" prefixed functions
#include "cc_val.h"
#include "cc_equ.h"
#include "cc_zero.h"
#include "cc_cap.h"
#include "cc_cen.h"
#include "cc_sea.h"
#include "cc_urb.h"
#include "cc_coun.h"
#include "cc_outl.h"
#include "cc_gbif.h"
#include "cc_inst.h"
#include "cc_iucn.h"
#include "cc_dupl.h"

using namespace Rcpp;

// [[Rcpp::export]]
List clean_coordinates_cpp(DataFrame x,
                           CharacterVector tests,
                           String lon_col = "decimalLongitude",
                           String lat_col = "decimalLatitude",
                           String species_col = "species",
                           Nullable<CharacterVector> countries_col = R_NilValue,
                           double capitals_rad = 10000.0,
                           double centroids_rad = 1000.0,
                           String centroids_detail = "both",
                           double inst_rad = 100,
                           String outliers_method = "quantile",
                           double outliers_mtp = 5,
                           double outliers_td = 1000,
                           int outliers_size = 7,
                           double range_rad = 0,
                           double zeros_rad = 0.5,
                           Nullable<DataFrame> capitals_ref = R_NilValue,
                           Nullable<DataFrame> centroids_ref = R_NilValue,
                           Nullable<DataFrame> country_ref = R_NilValue,
                           String country_refcol = "iso_a3",
                           Nullable<NumericVector> country_buffer = R_NilValue,
                           Nullable<DataFrame> inst_ref = R_NilValue,
                           Nullable<DataFrame> range_ref = R_NilValue,
                           Nullable<DataFrame> seas_ref = R_NilValue,
                           double seas_scale = 50,
                           Nullable<NumericVector> seas_buffer = R_NilValue,
                           Nullable<DataFrame> urban_ref = R_NilValue,
                           double aohi_rad = 1000,
                           bool verbose = true) {

  // Extract coordinates and species columns from the data
  NumericVector lon = x[lon_col];
  NumericVector lat = x[lat_col];
  CharacterVector species = x[species_col];
  int n = lon.size();

  // Initialize results matrix and summary vector
  LogicalMatrix results(n, tests.size());
  LogicalVector summary(n, true);

  // Step 1: Validate coordinates
  results(_, 0) = cc_val_cpp(lon, lat);

  // Stop if there are invalid coordinates
  if (is_true(any(!results(_, 0)))) {
    stop("Invalid coordinates detected. Please clean dataset before proceeding.");
  }

  // Prepare optional reference data if provided
  NumericMatrix cap_ref, cen_ref;
  bool cap_ref_provided = !Rf_isNull(capitals_ref);
  bool cen_ref_provided = !Rf_isNull(centroids_ref);
  if (cap_ref_provided) cap_ref = as<NumericMatrix>(capitals_ref);
  if (cen_ref_provided) cen_ref = as<NumericMatrix>(centroids_ref);

  // Prepare country reference data if provided
  DataFrame coun_ref;
  bool coun_ref_provided = !Rf_isNull(country_ref);
  if (coun_ref_provided) coun_ref = country_ref.get();

  // Sequential test execution
  for (int i = 0; i < tests.size(); i++) {
    std::string test = Rcpp::as<std::string>(tests[i]);

    if (test == "equal") {
      results(_, i) = cc_equ_cpp(lon, lat, "test");
    } else if (test == "zeros") {
      results(_, i) = cc_zero_cpp(lon, lat, zeros_rad);
    } else if (test == "capitals" && cap_ref_provided) {
      NumericMatrix points(n, 2);
      for (int j = 0; j < n; j++) {
        points(j, 0) = lon[j];
        points(j, 1) = lat[j];
      }
      results(_, i) = cc_cap_cpp(points, capitals_rad, true, cap_ref);
    } else if (test == "centroids" && cen_ref_provided) {
      results(_, i) = cc_cen_cpp(x, lon_col, lat_col, species_col, centroids_rad, true, centroids_detail, cen_ref, true, "clean", verbose);
    } else if (test == "seas" && !Rf_isNull(seas_ref)) {
      NumericMatrix coords(n, 2);
      for (int j = 0; j < n; j++) {
        coords(j, 0) = lon[j];
        coords(j, 1) = lat[j];
      }
      results(_, i) = cc_sea_cpp(coords, as<List>(seas_ref), seas_scale);
    } else if (test == "urban" && !Rf_isNull(urban_ref)) {
      NumericMatrix coords(n, 2);
      for (int j = 0; j < n; j++) {
        coords(j, 0) = lon[j];
        coords(j, 1) = lat[j];
      }
      results(_, i) = cc_urb_cpp(coords, as<List>(urban_ref), 0.0);
    } else if (test == "countries" && countries_col.isNotNull() && coun_ref_provided) {
      CharacterVector countries = x[Rcpp::as<std::string>(countries_col.get())];
      results(_, i) = as<LogicalVector>(cc_coun_cpp(x, lon_col, lat_col, country_refcol));
    } else if (test == "outliers") {
      results(_, i) = cc_outl_cpp(lon, lat, outliers_method, outliers_mtp, outliers_td, outliers_size, false);
    } else if (test == "gbif") {
      results(_, i) = cc_gbif_cpp(x, lon_col, lat_col);
    } else if (test == "institutions" && !Rf_isNull(inst_ref)) {
      DataFrame inst_df = as<DataFrame>(inst_ref.get());
      NumericVector inst_lon = inst_df["lon"];  // Adjusted for proper access
      NumericVector inst_lat = inst_df["lat"];
      results(_, i) = as<LogicalVector>(cc_inst_cpp(x, inst_lon, inst_lat, lon_col, lat_col, species_col, inst_rad));
    } else if (test == "range" && !Rf_isNull(range_ref)) {
      results(_, i) = as<LogicalVector>(cc_iucn_cpp(x, as<List>(range_ref), lon_col, lat_col, species_col, range_rad));
    } else if (test == "duplicates") {
      List additions;
      results(_, i) = cc_dupl_cpp(lon, lat, species, additions);
    }
  }

  // Create a summary
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < results.ncol(); j++) {
      if (!results(i, j)) {
        summary[i] = false;
        break;
      }
    }
  }

  // Return results and summary
  return List::create(
    Named("results") = results,
    Named("summary") = summary
  );
}

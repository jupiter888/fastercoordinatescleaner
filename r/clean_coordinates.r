#' Clean Geographic Coordinates
#' @name clean_coordinates
#' @title Main function for cleaning coordinates
#' @description This function cleans geographic coordinates using various tests implemented in C++.
#'
#' @param x A `data.frame` containing columns for longitude, latitude, and optionally country codes.
#' @param tests A character vector specifying which tests to apply. Options include "equal", "zeros", "capitals",
#'   "centroids", "seas", "urban", "countries", "outliers", "gbif", "institutions", "range", "duplicates".
#' @param lon_col Name of the longitude column. Defaults to `"decimalLongitude"`.
#' @param lat_col Name of the latitude column. Defaults to `"decimalLatitude"`.
#' @param species_col Name of the species column. Defaults to `"species"`.
#' @param countries_col (Optional) Name of the column with country codes. Set to `NULL` if not applicable.
#' @param capitals_rad Radius (in meters) around capitals for proximity checks. Default is `10000`.
#' @param centroids_rad Radius (in meters) around centroids for proximity checks. Default is `1000`.
#' @param centroids_detail String specifying details for the centroids test. Default is `"both"`.
#' @param inst_rad Radius (in meters) for institution proximity checks. Default is `100`.
#' @param outliers_method Method for detecting outliers. Default is `"quantile"`.
#' @param outliers_mtp Multiplier for the outliers method. Default is `5`.
#' @param outliers_td Threshold distance for the outliers method. Default is `1000`.
#' @param outliers_size Minimum occurrence count for outlier detection. Default is `7`.
#' @param range_rad Radius for the range check. Default is `0`.
#' @param zeros_rad Radius for zero-coordinate proximity checks. Default is `0.5`.
#' @param capitals_ref Reference data for capitals. Set to `NULL` if not applicable.
#' @param centroids_ref Reference data for centroids. Set to `NULL` if not applicable.
#' @param country_ref Reference data for countries. Set to `NULL` if not applicable.
#' @param country_refcol Column in `country_ref` used to match countries. Default is `"iso_a3"`.
#' @param country_buffer (Optional) Numeric vector for country buffer distances.
#' @param inst_ref Reference data for institutions. Set to `NULL` if not applicable.
#' @param range_ref Reference data for range check. Set to `NULL` if not applicable.
#' @param seas_ref Reference data for seas. Set to `NULL` if not applicable.
#' @param seas_scale Scaling factor for sea proximity checks. Default is `50`.
#' @param seas_buffer (Optional) Numeric vector for sea buffer distances.
#' @param urban_ref Reference data for urban areas. Set to `NULL` if not applicable.
#' @param aohi_rad Radius for areas of high interest. Default is `1000`.
#' @return A list with `results`, a logical matrix for each test per row, and `summary`, a logical vector indicating rows that passed all tests.
#' @param verbose Logical, if `TRUE`, outputs additional information during processing.
#' @export
#' @examples
#' # Load example data and reference data if needed
#' result <- clean_coordinates(
#'   sample_data,
#'   tests = c("capitals", "centroids", "countries"),
#'   capitals_ref = capitals_ref_data,
#'   centroids_ref = centroids_ref_data,
#'   country_ref = country_ref_data
#' )
clean_coordinates <- function(x,
                              tests,
                              lon_col = "decimalLongitude",
                              lat_col = "decimalLatitude",
                              species_col = "species",
                              countries_col = NULL,
                              capitals_rad = 10000.0,
                              centroids_rad = 1000.0,
                              centroids_detail = "both",
                              inst_rad = 100,
                              outliers_method = "quantile",
                              outliers_mtp = 5,
                              outliers_td = 1000,
                              outliers_size = 7,
                              range_rad = 0,
                              zeros_rad = 0.5,
                              capitals_ref = NULL,
                              centroids_ref = NULL,
                              country_ref = NULL,
                              country_refcol = "iso_a3",
                              country_buffer = NULL,
                              inst_ref = NULL,
                              range_ref = NULL,
                              seas_ref = NULL,
                              seas_scale = 50,
                              seas_buffer = NULL,
                              urban_ref = NULL,
                              aohi_rad = 1000,
                              verbose = TRUE) {
  # Ensure optional reference data is set to R_NilValue if not provided
  if (is.null(capitals_ref)) capitals_ref <- R_NilValue
  if (is.null(centroids_ref)) centroids_ref <- R_NilValue
  if (is.null(country_ref)) country_ref <- R_NilValue
  if (is.null(country_buffer)) country_buffer <- R_NilValue
  if (is.null(inst_ref)) inst_ref <- R_NilValue
  if (is.null(range_ref)) range_ref <- R_NilValue
  if (is.null(seas_ref)) seas_ref <- R_NilValue
  if (is.null(seas_buffer)) seas_buffer <- R_NilValue
  if (is.null(urban_ref)) urban_ref <- R_NilValue

  # Call the C++ function with parameters
  clean_coordinates_cpp(x, tests, lon_col, lat_col, species_col, countries_col,
                        capitals_rad, centroids_rad, centroids_detail, inst_rad,
                        outliers_method, outliers_mtp, outliers_td, outliers_size,
                        range_rad, zeros_rad, capitals_ref, centroids_ref,
                        country_ref, country_refcol, country_buffer, inst_ref,
                        range_ref, seas_ref, seas_scale, seas_buffer, urban_ref,
                        aohi_rad, verbose)
}

#' cc_coun Rcpp Wrapper
#'
#' @param x A data.frame containing coordinates and country codes.
#' @param lon The name of the longitude column. Default is "decimalLongitude".
#' @param lat The name of the latitude column. Default is "decimalLatitude".
#' @param iso3 The name of the country code column. Default is "countrycode".
#' @param value The return value type, either "clean" or "flagged". Default is "clean".
#' @param ref A SpatVector object representing the reference country polygons. Default is NULL.
#' @param ref_col The column name in the reference dataset containing the ISO codes. Default is "iso_a3".
#' @param verbose Logical, whether to print messages. Default is TRUE.
#' @param buffer Numeric, the buffer distance in meters. Default is NULL.
#'
#' @return A data.frame of cleaned coordinates or a logical vector of flags.
#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib FasterCoordinateCleaner
cc_coun <- function(x, 
                    lon = "decimalLongitude", 
                    lat = "decimalLatitude", 
                    iso3 = "countrycode",
                    value = "clean", 
                    ref = NULL, 
                    ref_col = "iso_a3",
                    verbose = TRUE,
                    buffer = NULL) {
  
  if (!iso3 %in% names(x)) {
    stop("iso3 argument missing, please specify")
  }
  
  if (verbose) {
    message("Testing country identity")
  }
  
  if (is.null(ref)) {
    if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
      stop("Install the 'rnaturalearth' package or provide a custom reference", call. = FALSE)
    }
    ref <- terra::vect(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"))
  } else {
    if (any(is(ref) == "Spatial") | inherits(ref, "sf")) {
      ref <- terra::vect(ref)
    }
    if (!(inherits(ref, "SpatVector") & terra::geomtype(ref) == "polygons")) {
      stop("ref must be a SpatVector with geomtype 'polygons'")
    }
    ref <- reproj(ref)
  }
  
  points <- as.matrix(x[, c(lon, lat)])
  iso3_codes <- as.character(x[[iso3]])
  ref_coords <- as.matrix(terra::geom(ref)[, c("x", "y")])
  ref_iso3 <- as.character(ref[[ref_col]])
  
  result <- cc_coun_cpp(points, iso3_codes, ref_coords, ref_iso3, buffer)
  
  if (verbose) {
    if (value == "clean") {
      message(sprintf("Removed %s records.", sum(!result)))
    } else {
      message(sprintf("Flagged %s records.", sum(!result)))
    }
  }
  
  switch(value, clean = return(x[result, ]), flagged = return(result))
}

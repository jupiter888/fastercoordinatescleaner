#' cc_urb Rcpp Wrapper
#'
#' @param x A data.frame containing coordinates.
#' @param lon The name of the longitude column. Default is "decimalLongitude".
#' @param lat The name of the latitude column. Default is "decimalLatitude".
#' @param ref A SpatVector object representing the urban areas. Default is NULL.
#' @param value The return value type, either "clean" or "flagged". Default is "clean".
#' @param verbose Logical, whether to print messages. Default is TRUE.
#'
#' @return A data.frame of cleaned coordinates or a logical vector of flags.
#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib FasterCoordinateCleaner
cc_urb <- function(x,
                   lon = "decimalLongitude",
                   lat = "decimalLatitude",
                   ref = NULL,
                   value = "clean",
                   verbose = TRUE) {

  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing urban areas")
  }

  if (is.null(ref)) {
    message("Downloading urban areas via rnaturalearth")
    ref <- try(suppressWarnings(terra::vect(
      rnaturalearth::ne_download(scale = 'medium',
                                 type = 'urban_areas',
                                 returnclass = "sf")
    )), silent = TRUE)

    if (inherits(ref, "try-error")) {
      warning(sprintf("Gazetteer for urban areas not found at\n%s",
                      rnaturalearth::ne_file_name(scale = 'medium',
                                                  type = 'urban_areas',
                                                  full_url = TRUE)))
      warning("Skipping urban test")
      switch(value, clean = return(x), flagged = return(rep(NA, nrow(x))))
    }
  } else {
    if (any(is(ref) == c("Spatial")) | inherits(ref, "sf")) {
      ref <- terra::vect(ref)
    }
    if (!(inherits(ref, "SpatVector") & terra::geomtype(ref) == "polygons")) {
      stop("ref must be a SpatVector with geomtype 'polygons'")
    }
    ref <- reproj(ref)
  }

  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"

  dat <- terra::vect(x[, c(lon, lat)],
                     geom = c(lon, lat),
                     crs = wgs84)
  limits <- terra::ext(dat) + 1
  ref <- terra::crop(ref, limits)
  ref <- terra::project(ref, wgs84)

  ref_coords <- as.matrix(terra::geom(ref)[, c("x", "y")])
  lon_col <- x[[lon]]
  lat_col <- x[[lat]]

  result <- cc_urb_cpp(lon_col, lat_col, ref_coords)

  if (verbose) {
    if (value == "clean") {
      message(sprintf("Removed %s records.", sum(!result)))
    } else {
      message(sprintf("Flagged %s records.", sum(!result)))
    }
  }

  switch(value, clean = return(x[result, ]), flagged = return(result))
}

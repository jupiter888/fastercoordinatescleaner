#' cc_iucn Rcpp Wrapper
#'
#' @param x A data.frame containing coordinates.
#' @param range A SpatVector of natural ranges for species.
#' @param lon The name of the longitude column. Default is "decimalLongitude".
#' @param lat The name of the latitude column. Default is "decimalLatitude".
#' @param species The name of the species column. Default is "species".
#' @param buffer The buffer distance in meters. Default is 0.
#' @param value The return value type, either "clean" or "flagged". Default is "clean".
#' @param verbose Logical, whether to print messages. Default is TRUE.
#'
#' @return A data.frame of cleaned coordinates or a logical vector of flags.
#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib FasterCoordinateCleaner
cc_iucn <- function(x,
                    range,
                    lon = "decimalLongitude",
                    lat = "decimalLatitude",
                    species = "species",
                    buffer = 0,
                    value = "clean",
                    verbose = TRUE){

  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing natural ranges")
  }

  if (any(is(range) == "Spatial") | inherits(range, "sf")) {
    range <- terra::vect(range)
  }
  if (!(inherits(range, "SpatVector") & terra::geomtype(range) == "polygons")) {
    stop("ref must be a SpatVector with geomtype 'polygons'")
  }

  if ("binomial" %in% names(range) & !species %in% names(range) & species %in% names(x)) {
    names(range)[names(range) == "binomial"] <- species
  }

  test_range <- range[[species]][, 1] %in% unique(unlist(x[, species]))
  range <- terra::subset(range, test_range)

  points <- as.matrix(x[, c(lon, lat)])
  species_col <- x[[species]]

  ranges <- lapply(split(range, range[[species]]), function(r) {
    list(coords = as.matrix(terra::geom(r)[, c("x", "y")]), species = unique(r[[species]]))
  })

  result <- cc_iucn_cpp(points, species_col, ranges, buffer)

  if (verbose) {
    if(value == "clean") {
      message(sprintf("Removed %s records.", sum(!result)))
    } else {
      message(sprintf("Flagged %s records.", sum(!result)))
    }
  }

  switch(value, clean = return(x[result, ]), flagged = return(result))
}

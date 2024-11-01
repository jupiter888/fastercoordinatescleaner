#' Identify Points in Sea Areas Using Rcpp
#'
#' Removes or flags coordinates located in the sea using a reference landmass.
#'
#' @param x A data frame containing species records with geographical coordinates.
#' @param lon The name of the longitude column in `x`. Default is "decimalLongitude".
#' @param lat The name of the latitude column in `x`. Default is "decimalLatitude".
#' @param ref A list of matrices representing the reference landmass polygons. If `NULL`, defaults to a predefined reference.
#' @param value Character, specifying the return value: "clean" for the records within the landmass, or "flagged" for a logical vector.
#' @param verbose Logical, indicating whether to print messages indicating progress. Default is TRUE.
#' @param buffer Distance of the buffer in meters to apply around land areas. Default is 0.0.
#'
#' @return A data frame or logical vector depending on the `value` argument.
#' @export
#' @useDynLib FasterCoordinateCleaner
cc_sea <- function(x, lon = "decimalLongitude", lat = "decimalLatitude", ref = NULL, value = "clean", verbose = TRUE, buffer = 0.0) {

  if (verbose) {
    message("Testing sea coordinates")
  }

  # Load default reference polygons if not provided
  if (is.null(ref)) {
    stop("You must provide a reference polygon for the landmass.")
  }

  # Extract coordinates from data frame
  coords <- as.matrix(x[, c(lon, lat)])

  # Call the Rcpp function
  result <- cc_sea_cpp(coords, ref, buffer)

  # Return results based on the value argument
  if (value == "clean") {
    return(x[result, ])
  } else if (value == "flagged") {
    return(result)
  } else {
    stop("Invalid 'value' argument.")
  }
}

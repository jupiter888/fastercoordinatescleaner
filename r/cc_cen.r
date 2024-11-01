#' Clean Coordinates: Centroids using optimized .rds data
#'
#' Removes or flags records within a certain radius around country centroids.
#'
#' @param x data.frame. Containing geographical coordinates and species names.
#' @param lon character string. The column with the longitude coordinates.
#' @param lat character string. The column with the latitude coordinates.
#' @param species character string. The column with the species identity.
#' @param buffer numeric. The buffer around each centroid, where records should be flagged as problematic. Default = 10000 (10 km).
#' @param geod logical. If TRUE, the radius around each centroid is calculated based on a sphere, buffer is in meters and independent of latitude. If FALSE, the radius is calculated assuming planar coordinates.
#' @param ref data.frame. Providing the reference coordinates for centroids. If NULL, uses the built-in reference data.
#' @param value character string. Defining the output value.
#' @param verbose logical. If TRUE, reports the name of the test and the number of records flagged.
#'
#' @return Depending on the `value` argument, either a `data.frame` containing the records considered correct by the test ("clean") or a logical vector ("flagged").
#' @export
cc_cen <- function(x, 
                   lon = "decimalLongitude", 
                   lat = "decimalLatitude",
                   species = "species",
                   buffer = 10000,
                   geod = TRUE,
                   ref = NULL, 
                   value = "clean", 
                   verbose = TRUE) {
  
  if (verbose) {
    message("Testing country centroids")
  }
  
  if (buffer > 10 & !geod) {
    warning("Using large buffer, check 'geod'")
  }
  if (buffer < 100 & geod) {
    warning("Using small buffer, check 'geod'")
  }
  
  # Select relevant columns from the input data
  if (!all(c(lon, lat) %in% colnames(x))) {
    stop("Longitude and latitude columns not found in the dataset.")
  }
  
  # Drop unused columns
  x <- drop_unused_columns(x, lon = lon, lat = lat, species = species, country = "countryCode")
  points <- as.matrix(x[, c(lon, lat)])
  
  # Load optimized .rds reference data
  if (is.null(ref)) {
    ref_data_path <- "/Users/njord888/Desktop/Myco_PKGs/New_Package/FasterCoordinateCleaner.Rcheck/00_pkg_src/FasterCoordinateCleaner/data/country_reference_crop.rds"
    if (!file.exists(ref_data_path)) {
      stop("Country reference .rds file not found. Please provide 'ref' argument.")
    }
    # Load the reference data from the .rds file
    ref_data <- readRDS(ref_data_path)
    ref_coords <- as.matrix(ref_data[, c("centroid.lon", "centroid.lat")])
  } else {
    ref_coords <- as.matrix(ref[, c(lon, lat)])
  }
  
  # Call the C++ function for distance checking
  out <- cc_cen_cpp(points, ref_coords, buffer, geod)
  
  if (verbose) {
    if (value == "clean") {
      message(sprintf("Removed %s records.", sum(!out)))
    } else {
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }
  
  # Return results
  switch(value, 
         clean = return(x[out, , drop = FALSE]), 
         flagged = return(out),
         stop("Invalid 'value' argument. Must be either 'clean' or 'flagged'.")
  )
}

#' @name cc_cap
#' @title Capital City Check Function
#' @param ref data.frame. Providing the reference coordinates for capital cities.
#'   If NULL, uses the built-in reference data.
#' @param verify logical. If TRUE, records are flagged only if they are the only
#'   flagged record for a given species. Default is FALSE.
#' @param value character string. Defining the output value. Either "clean" to return
#'   a data.frame with problematic records removed, or "flagged" to return a logical
#'   vector. Default = "clean".
#' @param verbose logical. If TRUE, prints messages during execution.
#'
#' @return Depending on the `value` argument, either a `data.frame`
#'   containing the records considered correct by the test ("clean") or a
#'   logical vector ("flagged").
#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib FasterCoordinateCleaner
cc_cap <- function(x,
                   lon = "decimalLongitude",
                   lat = "decimalLatitude",
                   species = "species",
                   buffer = 10000,
                   geod = TRUE,
                   ref = NULL,
                   verify = FALSE,
                   value = "clean",
                   verbose = TRUE) {

  # Input validation
  if (!is.data.frame(x)) {
    stop("Input 'x' must be a data.frame")
  }

  if (verbose) {
    message("Testing country capitals")
  }

  # Warning for large buffer with planar coordinates
  if (buffer > 10000 && !geod) {
    warning("Using large buffer (>10km) with planar coordinates. Consider using geod = TRUE")
  }

  # Check if required columns exist
  if (!all(c(lon, lat) %in% colnames(x))) {
    stop("Longitude and/or latitude columns not found in the dataset")
  }

  # Convert coordinates to matrix
  points <- try(as.matrix(x[, c(lon, lat)]), silent = TRUE)
  if (inherits(points, "try-error")) {
    stop("Could not convert coordinates to numeric matrix")
  }

  # Handle missing or invalid coordinates
  if (any(is.na(points))) {
    warning("Dataset contains missing coordinates")
    points[is.na(points)] <- 0  # Replace NA with 0 for processing
  }

  # Load reference data
  if (is.null(ref)) {
    ref_data_path <- system.file("data", "countryref.rda",
                                 package = "FasterCoordinateCleaner")
    if (!file.exists(ref_data_path)) {
      stop("Reference data not found. Please provide the 'ref' argument")
    }
    load(ref_data_path)
    if (!exists("countryref")) {
      stop("Could not load reference data from package")
    }
    ref_coords <- as.matrix(countryref[, c("capital.lon", "capital.lat")])
  } else {
    if (!is.matrix(ref) && !is.data.frame(ref)) {
      stop("Reference data must be a matrix or data.frame")
    }
    ref_coords <- as.matrix(ref)
    if (ncol(ref_coords) != 2) {
      stop("Reference data must contain exactly two columns (longitude and latitude)")
    }
  }

  # Call the C++ function for fast distance computation
  flagged <- try(cc_cap_cpp(points = points,
                            buffer = buffer,
                            geod = geod,
                            ref_coords = ref_coords), silent = TRUE)

  if (inherits(flagged, "try-error")) {
    stop("Error in C++ computation. Check input data format")
  }

  # Implement verification logic if required
  if (verify && sum(!flagged) > 0) {
    if (!species %in% colnames(x)) {
      stop("Species column not found in dataset")
    }
    flagged <- verify_records(x = x,
                              flags = flagged,
                              lon = lon,
                              lat = lat,
                              species = species)
  }

  # Print messages based on verbosity
  if (verbose) {
    n_flagged <- sum(!flagged)
    message(sprintf("Flagged %d record%s (%.1f%%)",
                    n_flagged,
                    if(n_flagged != 1) "s" else "",
                    100 * n_flagged / length(flagged)))
  }

  # Return results based on value parameter
  if (value == "clean") {
    return(x[flagged, , drop = FALSE])
  } else if (value == "flagged") {
    return(flagged)
  } else {
    stop("Invalid 'value' parameter. Must be either 'clean' or 'flagged'")
  }
}

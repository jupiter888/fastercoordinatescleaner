#' cc_val Rcpp Wrapper
#'
#' Identify Invalid lat/lon Coordinates as well as lat >90, lat <-90,
#' lon > 180 and lon < -180 are flagged.
#' This test is obligatory before running any further tests,
#' because tests only run provided with valid coordinates.
#'
#' Removes or flags non-numeric and not available coordinates
#' @param x A data.frame containing coordinates.
#' @param lon The name of the longitude column. Default is "decimalLongitude".
#' @param lat The name of the latitude column. Default is "decimalLatitude".
#' @param value The return value type, either "clean" or "flagged". Default is "clean".
#' @param verbose Logical, whether to print messages. Default is TRUE.
#'
#' @return A data.frame of cleaned coordinates or a logical vector of flags.
#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib FasterCoordinateCleaner
cc_val <- function(x,
                   lon = "decimalLongitude",
                   lat = "decimalLatitude",
                   value = "clean",
                   verbose = TRUE) {

  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing coordinate validity")
  }

  x[[lon]] <- suppressWarnings(as.numeric(as.character(x[[lon]])))
  x[[lat]] <- suppressWarnings(as.numeric(as.character(x[[lat]])))

  lon_col <- x[[lon]]
  lat_col <- x[[lat]]

  result <- cc_val_cpp(lon_col, lat_col)

  if (verbose) {
    if (value == "clean") {
      message(sprintf("Removed %s records.", sum(!result)))
    } else {
      message(sprintf("Flagged %s records.", sum(!result)))
    }
  }

  switch(value, clean = return(x[result, ]), flagged = return(result))
}

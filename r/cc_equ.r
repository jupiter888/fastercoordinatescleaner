#' cc_equ Rcpp Wrapper
#'
#' Removes or flags records with equal latitude and longitude coordinates,
#' either exact or absolute. Equal coordinates can often indicate data entry
#' errors.
#'
#' @param x A data.frame containing coordinates.
#' @param lon The name of the longitude column. Default is "decimalLongitude".
#' @param lat The name of the latitude column. Default is "decimalLatitude".
#' @param test A character string defining if coordinates are compared exactly ("identical") or on the absolute scale ("absolute"). Default is "absolute".
#' @param value The return value type, either "clean" or "flagged". Default is "clean".
#' @param verbose Logical, whether to print messages. Default is TRUE.
#'
#' @return A data.frame of cleaned coordinates or a logical vector of flags.
#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib FasterCoordinateCleaner
cc_equ <- function(x,
                   lon = "decimalLongitude",
                   lat = "decimalLatitude",
                   test = "absolute",
                   value = "clean",
                   verbose = TRUE) {

  match.arg(test, choices = c("absolute", "identical"))
  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing equal lat/lon")
  }

  lon_col <- x[[lon]]
  lat_col <- x[[lat]]

  result <- cc_equ_cpp(lon_col, lat_col, test)

  if (verbose) {
    if (value == "clean"){
      message(sprintf("Removed %s records.", sum(!result)))
    } else {
      message(sprintf("Flagged %s records.", sum(!result)))
    }
  }

  switch(value, clean = return(x[result, ]), flagged = return(result))
}

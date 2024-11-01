#' cc_zero Rcpp Wrapper
#'
#' @param x A data.frame containing coordinates.
#' @param lon The name of the longitude column. Default is "decimalLongitude".
#' @param lat The name of the latitude column. Default is "decimalLatitude".
#' @param buffer The buffer distance around the 0/0 point, in decimal degrees. Default is 0.5.
#' @param value The return value type, either "clean" or "flagged". Default is "clean".
#' @param verbose Logical, whether to print messages. Default is TRUE.
#'
#' @return A data.frame of cleaned coordinates or a logical vector of flags.
#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib FasterCoordinateCleaner
cc_zero <- function(x,
                    lon = "decimalLongitude",
                    lat = "decimalLatitude",
                    buffer = 0.5,
                    value = "clean",
                    verbose = TRUE) {

  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing zero coordinates")
  }

  if (buffer == 0) {
    buffer <- 0.00000000000001
  }

  lon_col <- x[[lon]]
  lat_col <- x[[lat]]

  result <- cc_zero_cpp(lon_col, lat_col, buffer)

  if (verbose) {
    if (value == "clean") {
      message(sprintf("Removed %s records.", sum(!result)))
    } else {
      message(sprintf("Flagged %s records.", sum(!result)))
    }
  }

  switch(value, clean = return(x[result, ]), flagged = return(result))
}

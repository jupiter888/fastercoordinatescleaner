#' cc_gbif Rcpp Wrapper
#'
#' @param x A data.frame containing coordinates.
#' @param lon The name of the longitude column. Default is "decimalLongitude".
#' @param lat The name of the latitude column. Default is "decimalLatitude".
#' @param species The name of the species column. Default is "species".
#' @param buffer The buffer distance in meters. Default is 1000.
#' @param geod Logical, whether to use geodetic calculations. Default is TRUE.
#' @param verify Logical, whether to verify the results. Default is FALSE.
#' @param value The return value type, either "clean" or "flagged". Default is "clean".
#' @param verbose Logical, whether to print messages. Default is TRUE.
#'
#' @return A data.frame of cleaned coordinates or a logical vector of flags.
#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib FasterCoordinateCleaner
cc_gbif <- function(x,
                    lon = "decimalLongitude",
                    lat = "decimalLatitude",
                    species = "species",
                    buffer = 1000,
                    geod = TRUE,
                    verify = FALSE,
                    value = "clean",
                    verbose = TRUE) {

  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing GBIF headquarters, flagging records around Copenhagen")
  }

  if (buffer > 10 & !geod) {
    warning("Using large buffer, check 'geod'")
  }
  if (buffer < 100 & geod) {
    warning("Using small buffer, check 'geod'")
  }

  lon_col <- x[[lon]]
  lat_col <- x[[lat]]

  result <- cc_gbif_cpp(lon_col, lat_col, buffer, geod)

  if (verbose) {
    if(value == "clean") {
      message(sprintf("Removed %s records.", sum(!result)))
    } else {
      message(sprintf("Flagged %s records.", sum(!result)))
    }
  }

  switch(value, clean = return(x[result, ]), flagged = return(result))
}

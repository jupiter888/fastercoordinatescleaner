#' cc_dupl Rcpp Wrapper
#'
#' @param x A data.frame containing coordinates, species names, and additional columns.
#' @param lon The name of the longitude column. Default is "decimalLongitude".
#' @param lat The name of the latitude column. Default is "decimalLatitude".
#' @param species The name of the species column. Default is "species".
#' @param additions A vector of character strings for additional columns. Default is NULL.
#' @param value The return value type, either "clean" or "flagged". Default is "clean".
#' @param verbose Logical, whether to print messages. Default is TRUE.
#'
#' @return A data.frame of cleaned coordinates or a logical vector of flags.
#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib FasterCoordinateCleaner
cc_dupl <- function(x,
                    lon = "decimalLongitude",
                    lat = "decimalLatitude",
                    species = "species",
                    additions = NULL,
                    value = "clean",
                    verbose = TRUE) {

  if (verbose) {
    message("Testing duplicates")
  }

  lon_col <- x[[lon]]
  lat_col <- x[[lat]]
  species_col <- x[[species]]

  additions_list <- list()
  if (!is.null(additions)) {
    for (addition in additions) {
      additions_list[[addition]] <- x[[addition]]
    }
  }

  result <- cc_dupl_cpp(lon_col, lat_col, species_col, additions_list)

  if (verbose) {
    if(value == "clean"){
      message(sprintf("Removed %s records.", sum(!result)))
    }else{
      message(sprintf("Flagged %s records.", sum(!result)))
    }
  }

  switch(value, clean = return(x[result, ]), flagged = return(result))
}

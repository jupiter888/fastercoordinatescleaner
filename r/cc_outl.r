#' Identify Geographic Outliers in Species Distributions
#'
#' Flags geographic outliers in species distributions based on specified methods:
#' "quantile", "mad", "distance", or "intrinsic" for intrinsic vectorization.
#'
#' @param x A data frame containing species records.
#' @param lon The column name for longitude. Default is "decimalLongitude".
#' @param lat The column name for latitude. Default is "decimalLatitude".
#' @param species The column name for species. Default is "species".
#' @param method The method for outlier detection: "quantile", "mad", "distance", or "intrinsic".
#' @param mltpl Multiplier for IQR or MAD. Default is 5.
#' @param tdi Threshold distance for "distance" method. Default is 1000.
#' @param min_occs Minimum number of occurrences required for testing. Default is 7.
#' @param value The return value type: "clean", "flagged", or "ids". Default is "clean".
#' @param verbose Whether to display messages. Default is TRUE.
#' @param intrinsic Whether to use intrinsic vectorization for large datasets.
#'
#' @return A cleaned data frame, logical vector, or vector of row indices depending on \code{value}.
#' @export
#' @useDynLib FasterCoordinateCleaner
cc_outl <- function(x,
                    lon = "decimalLongitude",
                    lat = "decimalLatitude",
                    species = "species",
                    method = "quantile",
                    mltpl = 5,
                    tdi = 1000,
                    min_occs = 7,
                    value = "clean",
                    verbose = TRUE,
                    intrinsic = FALSE) {

  # Call the Rcpp function
  result <- cc_outl_cpp(x[[lon]], x[[lat]], method, mltpl, tdi, min_occs, intrinsic)

  if (value == "clean") {
    return(x[!result, ])
  } else if (value == "flagged") {
    return(result)
  } else if (value == "ids") {
    return(which(result))
  }
}

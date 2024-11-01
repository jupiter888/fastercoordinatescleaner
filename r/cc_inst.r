#' cc_inst Rcpp Wrapper
#'
#' @param x A data.frame containing coordinates.
#' @param lon The name of the longitude column. Default is "decimalLongitude".
#' @param lat The name of the latitude column. Default is "decimalLatitude".
#' @param species The name of the species column. Default is "species".
#' @param buffer The buffer distance in meters. Default is 100.
#' @param geod Logical, whether to use geodetic calculations. Default is FALSE.
#' @param ref A SpatVector object representing the reference biodiversity institutions. Default is NULL.
#' @param verify Logical, whether to verify the results. Default is FALSE.
#' @param verify_mltpl Numerical, factor by which the verify buffer exceeds the initial buffer. Default is 10.
#' @param value The return value type, either "clean" or "flagged". Default is "clean".
#' @param verbose Logical, whether to print messages. Default is TRUE.
#'
#' @return A data.frame of cleaned coordinates or a logical vector of flags.
#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib FasterCoordinateCleaner
cc_inst <- function(x,
                    lon = "decimalLongitude",
                    lat = "decimalLatitude",
                    species = "species",
                    buffer = 100,
                    geod = FALSE,
                    ref = NULL,
                    verify = FALSE,
                    verify_mltpl = 10,
                    value = "clean",
                    verbose = TRUE) {

  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing biodiversity institutions")
  }

  if (buffer > 10 & !geod) {
    warning("Using large buffer, check 'geod'")
  }
  if (buffer < 100 & geod) {
    warning("Using small buffer, check 'geod'")
  }

  lon_col <- x[[lon]]
  lat_col <- x[[lat]]

  if (is.null(ref)) {
    ref <- CoordinateCleaner::institutions
    ref <- ref[!is.na(ref$decimalLongitude) & !is.na(ref$decimalLatitude), ]
  }

  ref_coords <- as.matrix(ref[, c("decimalLongitude", "decimalLatitude")])

  result <- cc_inst_cpp(lon_col, lat_col, ref_coords, buffer, geod)

  if (verify) {
    ref_in <- x[!result, ]

    if (nrow(ref_in) > 0) {
      if (geod) {
        dg <- seq(from = 0, to = 360, by = 5)

        buff_XY <- geosphere::destPoint(p = ref_in[, c(lon, lat)],
                                        b = rep(dg, each = nrow(ref_in)),
                                        d = buffer * verify_mltpl)

        id <- rep(seq(from = 1, to = nrow(ref_in)), times = length(dg))
        lst <- split(data.frame(buff_XY), f = id)

        lst <- lapply(lst, as.matrix)
        poly <- lapply(lst, terra::vect, crs = wgs84, type = "polygons")
        ref <- terra::vect(ref)

        ref$species <- ref_in[, species]
      } else {
        ref <- terra::vect(ref_in, geom = c(lon, lat))
        ref <- terra::buffer(ref, width = buffer * verify_mltpl)
        ref$species <- ref_in[, species]
      }

      f_spec <- x[x[, species] %in% ref$species, ]

      dbch_flag <- logical(nrow(ref_in))

      for (i in seq_len(nrow(ref_in))) {
        dbch <- terra::extract(terra::vect(f_spec[unlist(f_spec[species]) == ref$species[i], ], geom = c(lon, lat), crs = wgs84),
                               terra::subset(ref, seq_len(length(ref)) == i))

        dbch_flag[i] <- sum(!is.na(dbch$species)) > nrow(ref_in[ref_in[[species]] == ref_in[[i, species]] & ref_in[[lon]] ==  ref_in[[i, lon]] & ref_in[[lat]] ==  ref_in[[i, lat]], ])
      }

      result[rownames(ref_in)] <- unlist(dbch_flag)
    }
  }

  if (verbose) {
    if (value == "clean") {
      message(sprintf("Removed %s records.", sum(!result)))
    } else {
      message(sprintf("Flagged %s records.", sum(!result)))
    }
  }

  switch(value, clean = return(x[result, ]), flagged = return(result))
}

# streets ----

#' Transform for network streets
#'
#' @param x clickhouse single_source
#' @param cell_size cell_size of regular target rast, measured in metres
#' @param ... other parameters
#'
transform.streets <- function(x, cell_size = 50, ...) {
  ### Prepare SpatRaster object ###
  berlin_rast <- create_berlin_rast(cell_size)
  ### Prepare streets ###
  streets_vect <- prepare_streets(x$dat)
  strassenklassen <- factor(streets_vect$strassenklasse) %>%
    levels()

  ### Calculate street lengths for each strassenklasse ###
  for (i in strassenklassen) {
    logging("~~~~~~ Starting street type %s. ~~~~~~", i)

    streets_vect_filtered <-
      streets_vect[streets_vect$strassenklasse == i, ]
    street_rast <- calc_line_length_per_cell(berlin_rast,
                                             streets_vect_filtered)
    names(street_rast) <- paste0("strassenklasse_", i)
    rm(streets_vect_filtered)
    gc()

    add(berlin_rast) <- street_rast
    rm(street_rast)
    gc()

    logging("~~~~~~ Finishing street type %s. ~~~~~~", i)

  }

  ### Transform SpatRaster into df ###
  streets_df <- rast_to_df(berlin_rast)

  x$dat <- streets_df

  x
}


#' Create terra SpatRaster object with line lengths per cell
#'
#' raster_obj and lines_obj need to be in the same crs, preferibly with a
#' crs that has meteres as units.
#'
#' @param rast_obj terra raster object, e.g. output of create_berlin_rast()
#' @param lines_vect_obj terra SpatVector object with geometry lines
#'
#' @return terra SpatRaster object in crs ESPG:25833
#'
#' @export
calc_line_length_per_cell <- function(rast_obj,
                                      lines_vect_obj) {
  #~~~ Object Check ~~~
  stopifnot(
    "SpatRaster" %in% class(rast_obj),
    "SpatVector" %in% class(lines_vect_obj),
    "lines" %in% geomtype(lines_vect_obj)
  )
  #~~~ CRS Check ~~~
  stopifnot(crs(rast_obj) == crs(lines_vect_obj))

  logging(
    "           Starting intersection calculation of %s lines with %s cells.",
    nrow(lines_vect_obj),
    ncell(rast_obj)
  )
  rast_obj_with_length <- rasterizeGeom(lines_vect_obj, rast_obj,
                                        fun = "length",
                                        unit = "m")
  rm(lines_vect_obj, rast_obj)
  gc()

  return(rast_obj_with_length)
}

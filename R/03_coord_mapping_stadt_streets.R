# coord_mapping_stadt_streets ----

#' Transform for coord_mapping_stadt_streets
#'
#' @param x clickhouse single_source
#' @param cell_size cell_size of regular target rast, measured in metres
#' @param ... other parameters
#'
transform.coord_mapping_stadt_streets <-
  function(x, cell_size = 50, ...) {
    # fix linting
    berlin <-
      y <- stadt_x <- stadt_y <- streets <- element_nr <- NULL
    geometry <- . <- NULL
    
    ### Prepare SpatRaster object ###
    berlin_rast <- create_berlin_rast(cell_size)
    
    ### Prepare streets ###
    logging("~~~ Rounding street coordinates. ~~~",)
    streets_raw <- x$dat %>% st_transform(25833)
    streets <- round_line_coordinates(streets_raw, decimals = 2)
    
    streets$ID <-
      1:nrow(streets) # Add running ID for merge after intersection
    
    streets_vect <- vect(streets)
    
    ### Intersect lines with raster ###
    logging(
      "~~~ Intersecting %s lines (=streets) with %s raster cells ~~~",
      nrow(streets_vect),
      ncell(berlin_rast)
    )
    # Careful: there is a tidyr::extract function
    mapping_stadt_streets_raw <-
      terra::extract(berlin_rast, streets_vect, xy = T)
    
    mapping_stadt_streets <- mapping_stadt_streets_raw %>%
      rename(stadt_x = x,
             stadt_y = y) %>%
      merge(x = streets, y = ., by = "ID") %>%
      arrange(element_nr) %>%
      sf_to_geojson_df() %>%
      select(element_nr, geometry, stadt_x, stadt_y)
    
    logging(
      "~~~ Found %s intersections between lines (=streets) and raster. ~~~",
      nrow(mapping_stadt_streets)
    )
    
    x$dat <- mapping_stadt_streets
    
    x
  }

#' Rounds coordinates of linestring sf object
#'
#' @param lines_sf spatial sf object of lines (Linestring) in crs 25833
#' @param decimals integer - number of decimals to keep. Defaults to 2 decimals.
#' Precision: 1 decimal = 10cm, 2 decimals = 1cm.
#' @return spatial sf object of lines with rounded geometry column
#'
#' @export
round_line_coordinates <- function(lines_sf, decimals = 2) {
  sf_obj_check(lines_sf)
  stopifnot(st_crs(lines_sf) == st_crs(25833))
  
  for (i in 1:nrow(lines_sf)) {
    coords <- st_coordinates(lines_sf$geometry[i])[, c("X", "Y")]
    coords <- round(coords, decimals)
    lines_sf$geometry[i] <- st_linestring(coords, dim = "XY")
  }
  
  return(lines_sf)
}

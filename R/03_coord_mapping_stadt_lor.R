# coord_mapping_stadt_lor ----

#' Transform for coord_mapping_stadt_lor
#'
#' @param x clickhouse single_source
#' @param cell_size cell_size of regular target rast, measured in metres
#' @param ... other parameters
#'
transform.coord_mapping_stadt_lor <-
  function(x, cell_size = 50, ...) {
    # fix linting
    berlin <- y <- stadt_x <- stadt_y <- lor <- element_nr <- NULL
    geometry <- . <- NULL
    PLR_ID <- NULL
    
    ### Prepare SpatRaster object ###
    berlin_rast <- create_berlin_rast(cell_size)
    
    ### Prepare lor ###
    lor_wgs84 <- x$dat
    # Correct CRS has to be set manually, because raw data comes from db
    # without CRS information - which leads to default assignment of CRS WGS84.
    # We overwrite the CRS with the correct one. The geometries are in correct
    # CRS already, hence no st_transform is needed.
    lor <- st_sf(lor_wgs84, crs = 25833)
    
    # Make sure LOR PLR_IDs are actually unique:
    stopifnot(length(unique(lor$PLR_ID)) == nrow(lor))
    # Make sure CRS is nor actually correct:
    stopifnot(st_crs(lor) == st_crs(25833))
    
    lor$ID <-
      1:nrow(lor) # Add running ID for merge after intersection
    
    lor_vect <-
      vect(lor) # Turn lor into a SpatVector Object to work in terra
    
    ### Intersect lines with raster ###
    logging(
      "~~~ Intersecting %s multipolygons (=lor) with %s raster cells ~~~",
      nrow(lor_vect),
      ncell(berlin_rast)
    )
    # Careful: there is a tidyr::extract function
    mapping_stadt_lor_raw <-
      terra::extract(berlin_rast, lor_vect, xy = T)
    
    mapping_stadt_lor <- mapping_stadt_lor_raw %>%
      rename(stadt_x = x,
             stadt_y = y) %>%
      merge(x = lor, y = ., by = "ID") %>%
      arrange(PLR_ID) %>%
      sf_to_geojson_df() %>%
      select(PLR_ID, stadt_x, stadt_y) # on purpose without geometries
    
    logging(
      "~~~ Found %s intersections between multipolygons (=lor) and raster. ~~~",
      nrow(mapping_stadt_lor)
    )
    
    x$dat <- mapping_stadt_lor
    
    x
  }

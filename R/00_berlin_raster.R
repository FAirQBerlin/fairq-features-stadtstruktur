#' Create stars raster for Berlin
#'
#' This function creates the target raster (=grid) for Berlin.
#' Default values create a grid of 50m x 50m cells.
#'
#' @param cell_size (numeric) How wide and high (horizontal and vertical extension) 
#'                  a cell of the resulting stars raster should be?
#'                  Measured in metres. Defaults to 50m.
#' @param target_crs (int) What crs (= Coordinate Reference System) should
#'                   the resulting stars raster have? Defaults to EPSG:25833
#'                   (https://epsg.io/25833), which constitutes the standard on Berlin's
#'                   Fisbroker Geoservers. Conveniently, EPSG:25833 uses metres as units.
#'
#' @return stars object - raster of berlin
#'
#' @export
create_berlin_stars <- function(cell_size = 50,
                                target_crs = 25833) {
  # Get border of Berlin as one polygon
  berlin_sf <- get_berlin_bezirke() %>%
    # Make one polygon of all boroughs.
    st_union() %>% 
    # Transform that units are in metres.
    st_transform("EPSG:25833") %>% 
    # Add a buffer around Berlin, so that stars raster
    # captures the entirety of Berlin.
    st_buffer(dist = cell_size) 
  
  # Create stars raster based on Berlin's border
  berlin_stars <- st_as_stars(
    st_bbox(berlin_sf),
    dx = cell_size,
    dy = cell_size,
    pretty = TRUE,
    crs = st_crs("EPSG:25833")
  ) %>%
    st_crop(berlin_sf) # crop to berlin
  
  if (target_crs != 25833) {
    # Reproject
    berlin_stars <- st_transform(berlin_stars,
                                 st_crs(target_crs))
  }
  
  return(berlin_stars)
}


#' Create Berlin terra "SpatRaster" object
#'
#' @param cell_size (numeric) How wide and high a cell of the
#'                   resulting "SpatRaster" should be? Measured in metres.
#'                   Defaults to 50m.
#' @return SpatRaster object in crs ESPG:25833
#'
#' @export
create_berlin_rast <- function(cell_size = 50) {
  berlin_rast <- create_berlin_stars(cell_size) %>%
    rast()
  
  berlin_rast <- init(berlin_rast, 1:ncell(berlin_rast))
  names(berlin_rast) <- "id"
  
  return(berlin_rast)
  
}


#' Get the Berlin boroughs as an sf object (polygons)
#'
#' @return sf object - 12 polygons of the Berlin boroughs
#'
#' @export
get_berlin_bezirke <- function() {
  extract_raw_clickhouse("berlin_bezirke")
}

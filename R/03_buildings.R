# buildings ----

#' Transform for buildings
#'
#' @param x clickhouse single_source
#' @param cell_size cell_size of regular target rast, measured in metres
#' @param ... other parameters
#'
transform.buildings <- function(x, cell_size = 50, ...) {
  ### Prepare SpatRaster object ###
  berlin_rast <- create_berlin_rast(cell_size)
  
  ### Prepare buildings ###
  logging("~~~ Preparing 'building density' and 'buildings height' ~~~")
  buildings_density_vect <- prepare_buildings_density(x$dat)
  buildings_height_vect <- extract_raw_clickhouse("buildings_height") %>%
    prepare_buildings_height()
  
  ### Rasterize buildings ###
  logging(
    "~~~ Starting calculation of 'building density':
          rasterizing %s buildings on %s raster cells. ~~~",
    nrow(buildings_density_vect),
    ncell(berlin_rast)
  )
  buildings_density_rast <- calc_area_weighted_values(berlin_rast,
                                                      buildings_density_vect,
                                                      "density")
  
  logging(
    "~~~ Starting calculation of 'building height':
          rasterizing %s buildings on %s raster cells. ~~~",
    nrow(buildings_height_vect),
    ncell(berlin_rast)
  )
  buildings_height_rast <- calc_area_weighted_values(berlin_rast,
                                                     buildings_height_vect,
                                                     "height")
  
  ### Combine both layers in one SpatRaster ###
  buildings_rast <- c(buildings_density_rast, buildings_height_rast)
  
  ### Transform SpatRaster into df ###
  buildings_df <- rast_to_df(buildings_rast)
  
  x$dat <- buildings_df
  
  x
}


#' Prepare raw building density data
#'
#' @param buildings_density_raw sf object of raw building density (polygon) data
#' @param as_vect bool, shall return value be a terra SpatVect object?
#'                Defaults to TRUE. If false, an sf object is returned.
#' @return SpatVect terra object in crs ESPG:25833, sf object if
#'         as_vect = FALSE
#'
#' @export
prepare_buildings_density <- function(buildings_density_raw, as_vect = TRUE) {
  
  density <- gfz_19_2 <- grz_19_2 <- NULL # fix linting
  
  buildings_density <- buildings_density_raw %>%
    st_transform(25833) %>%
    mutate(density = gfz_19_2 / grz_19_2) %>%
    filter(!is.na(density) & !is.infinite(density)) %>%
    select(density)
  
  if (as_vect == TRUE) {
    buildings_density <- buildings_density %>%
      vect()
  }
  
  return(buildings_density)
}


#' Prepare raw building height data
#'
#' @param buildings_height_raw sf object of raw building height (polygon) data
#' @param as_vect bool, shall return value be a terra SpatVect object?
#'                Defaults to TRUE. If false, an sf object is returned.
#' @return SpatVect terra object in crs ESPG:25833, sf object if
#'         as_vect = FALSE
#'
#' @export
prepare_buildings_height <-
  function(buildings_height_raw, as_vect = TRUE) {
    height <- NULL # fix linting
    
    buildings_height <- buildings_height_raw %>%
      st_transform(25833) %>%
      rename(height = "hoehe")
    
    if (as_vect == TRUE) {
      buildings_height <- buildings_height %>%
        vect()
    }
    
    return(buildings_height)
  }

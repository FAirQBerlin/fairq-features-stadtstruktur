
#' Turns a terra SpatRaster Object into a data.frame.
#'
#' Turns a terra SpatRaster Object into a data.frame while loosing the crs
#' (= coordinate reference system) info is lost. One former raster cell is
#' represented as one row with centroid coordinates.
#'
#' @param spat_raster_obj (spatRaster) a spatRaster object (terra package).
#' @param add_id (logical) should a running id be added to data.frame as the
#'               first column?
#' @param na_rm (logical) If TRUE, cells that have a NA value in at least one
#'               layer are removed. Defaults to TRUE.
#' @return data.frame with coordinate columns x and y. These represent the
#'         centroids of the raster cells. Values of the SpatRaster layers are
#'         represented in the values column. No crs info in this return object!
#'
#' @export
rast_to_df <- function(spat_raster_obj, add_id = TRUE, na_rm = TRUE) {
  stopifnot("SpatRaster" %in% class(spat_raster_obj))
  
  raster_as_df <- as.data.frame(spat_raster_obj,
                                optional = TRUE,
                                xy = TRUE,
                                na.rm = na_rm)
  
  id <- NULL # fix R build error
  
  if (add_id == TRUE) {
    raster_as_df <- raster_as_df %>%
      select(-any_of("id")) %>%
      # id cannot be running number, because for not complete rasters
      # we want to keep the original id
      mutate(id = row.names(raster_as_df) %>%
               as.integer()) %>%
      relocate(id)
  }
  
  return(raster_as_df)
}


#' Transforms a data.frame into a terra SpatRaster object.
#'
#' @param df (data.frame) As created by rast_to_df()
#' @param crs (integer) EPSG id for crs, defaults to 25833
#' @param drop_id (bool)
#' @return (spatRaster) A terra spatRaster object .
#'
#' @export
df_to_rast <- function(df, crs = 25833, drop_id = TRUE) {
  # These cols must be present in df to make transformation work:
  id_col_names <- c("x", "y", "id")
  
  stopifnot("data.frame" %in% class(df) &
              all(id_col_names %in% names(df)))
  
  cols <- names(df)
  layer_col_names <- cols[!(cols %in% id_col_names)]
  # Correct order of cols is needed for correct transformation
  rast_cols <- c(id_col_names, layer_col_names)
  
  spat_raster_obj <- rast(df %>% select(all_of(rast_cols)),
                          type = "xyz",
                          crs = c(st_crs(crs))$wkt)
  
  layer_names <- names(spat_raster_obj)
  
  if (drop_id) {
    layer_names <- layer_names[!layer_names %in% "id"]
  }
  
  return(spat_raster_obj[[layer_names]])
  
}


#' Sample points from lines while keeping attributes
#'
#' @param lines_sf_obj sf object (lines)
#' @param n numeric, number of points to be sampled from lines per metre (if crs
#'          unit is metres)
#' @param points shall return value be of geometry_type POINTS? Defaults to TRUE.
#'               If FALSE, return value me be of geometry_type MULTIPOINTS
#'
#' @return sf object (points) with attributes of the input lines sf obj
#'
#' @export
lines_to_points <- function(lines_sf_obj,
                            n = 1,
                            points = TRUE) {
  st_geometry <- m <- NULL # fix linting
  
  #~~~ Object Check ~~~
  line_type <- st_geometry_type(lines_sf_obj) %>% unique()
  unit <- st_crs(lines_sf_obj)$units
  
  stopifnot(line_type %in% c("LINESTRING", "MULTILINESTRING"))
  if (unit != "m") {
    warning(paste("Caution: Unit is not metres, but", unit))
  }
  
  if ("MULTILINESTRING" %in% line_type) {
    # If lines_sf_obj is MULTILINESTRING, we turn it into LINESTRING:
    # Otherwise point sampling does not work
    # Suppress warning because "repeating attributes" is what we want
    suppressWarnings(lines_sf_obj <- lines_sf_obj %>%
                       st_cast("LINESTRING"))
  }
  
  # Turn line geometries into points (1 point every 1m)
  sf::st_geometry(lines_sf_obj) <- "geometry"
  points_geometries_sf <- st_line_sample(lines_sf_obj,
                                         density = set_units(n, 1 / m))
  
  # Combine attributes of lines with the newly created point geometries
  points_sf <- st_sf(lines_sf_obj,
                     geometry = points_geometries_sf)
  
  if (points == TRUE) {
    # Suppress warning because "repeating attributes" is what we want
    suppressWarnings(points_sf <- points_sf %>%
                       # Convert MULTIPOINT to POINT: terra::rasterize can't handle MULTIPOINT
                       st_cast("POINT"))
  }
  
  return(points_sf)
}


#' Rasterize polygons - calculate area weighted values per cell
#'
#' raster_obj and poly_vect_obj need to be in the same crs, preferibly with a
#' crs that has meteres as units.
#'
#' @param rast_obj terra raster object, e.g. output of create_berlin_rast()
#' @param poly_vect_obj terra SpatVector object with geometry polygons
#' @param layer layer name that contains the values to be rasterized
#'
#' @return terra SpatRaster object in crs ESPG:25833
#'
#' @export
calc_area_weighted_values <-
  function(rast_obj, poly_vect_obj, layer) {
    #~~~ Object Check ~~~
    stopifnot(
      "SpatRaster" %in% class(rast_obj),
      layer %in% names(poly_vect_obj),
      "SpatVector" %in% class(poly_vect_obj),
      "polygons" %in% geomtype(poly_vect_obj)
    )
    #~~~ CRS Check ~~~
    stopifnot(crs(rast_obj) == crs(poly_vect_obj))
    
    names(poly_vect_obj) <-
      sub(layer, "original_values", names(poly_vect_obj))
    # We need to start with all NA for extract to work:
    rast_obj <- init(rast_obj, NA)
    extracted <- terra::extract(rast_obj,
                                poly_vect_obj,
                                exact = TRUE,
                                cells = TRUE)
    extracted$value <- poly_vect_obj$original_values[extracted$ID] *
      extracted$fraction
    
    aggregated <- aggregate(extracted[, c("value", "fraction")],
                            extracted[, "cell", drop = FALSE],
                            sum)
    rast_obj[aggregated$cell] <-
      aggregated$value / aggregated$fraction
    
    # Last layer is the one we modified:
    names(rast_obj)[length(names(rast_obj))] <- layer
    
    # nolint start
    #
    # If we want to remove NAs with 0s:
    # rast_obj[is.na(rast_obj)] <- 0 # Replace NAs with 0s
    #
    # Visual check if rasterization is plausible
    # dev.off()
    # plot(poly_vect_obj)
    # plot(rast_obj, add = T)
    # plot(rast_obj["density"], add = T)
    # plot(poly_vect_obj, y = 1, add = T, legend = "topleft")
    # text(rast_obj["density"], cex = 0.8)
    #
    # nolint end
    
    return(rast_obj)
  }


#' Turns sf object into a dataframe
#'
#' Turns the geometry of a sf_obj into a character column, in the form of a geojson.
#'
#' @param sf_obj spatial sf object
#' @return dataframe with a geometry column that is of type character (geojson)
#'
#' @export
sf_to_geojson_df <- function(sf_obj) {
  sf_obj_check(sf_obj)
  
  geometries <- sfc_geojson(sf_obj$geometry)
  # Turn sf_obj into a NON sf_obj (just dropping column does not work)
  df <- st_set_geometry(sf_obj, NULL)
  df$geometry <- c(geometries)
  
  return(df)
}


#' Checks if obj is a spatial sf object.
#'
#' Passes if obj is a spatial sf object, throws error otherwise.
#'
#' @param obj any object
#'
#' @export
sf_obj_check <- function(obj) {
  if (!"sf" %in% class(obj)) {
    stop("Argument provided is not an sf object, thus cannot be processed.")
  }
}

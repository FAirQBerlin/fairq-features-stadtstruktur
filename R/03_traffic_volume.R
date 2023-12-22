
#' Transform for traffic_volume
#'
#' @param x clickhouse single_source
#' @param cell_size cell_size of regular target rast, measured in metres
#' @param ... other parameters
#'
transform.traffic_volume <- function(x, cell_size = 50, ...) {
  
  ### Prepare SpatRaster object ###
  berlin_rast <- create_berlin_rast(cell_size)
  
  ### Prepare exact and average traffic volumes as sf objects ###
  traffic_volume_exact <- x$dat %>%
    prepare_traffic()
  traffic_volume_avg <- get_traffic_volume_avg()
  
  ### Rasterize traffic volume ###
  r_traffic <- calc_traffic_volume(traffic_volume_exact,
                                   traffic_volume_avg,
                                   berlin_rast)
  
  x$dat <- r_traffic %>%
    rast_to_df()
  
  x
}


#' Prepare raw exact traffic volume for rasterization
#' 
#' @param traffic_raw sf object, "Verkehrsmengen" (=traffic volume) from raw db
#' @param vehicle_type character, either "kfz" or "lkw"
#'
#' @return sf object (lines), representing exact traffic volumes
#'
#' @export
prepare_traffic <- function(traffic_raw, vehicle_type = "kfz") {
  traffic <- traffic_raw %>%
    mutate_if(is.character, as.factor) %>%
    select(any_of(c("strassenklasse", vehicle_type))) %>%
    st_transform(25833)
  
  return(traffic)
}


#' Get average traffic volume as sf object (streets as lines)
#'
#' @param vehicle_type character, either "kfz" or "lkw"
#'
#' @return sf object (lines), all streets
#'
#' @export
get_traffic_volume_avg <- function(vehicle_type = "kfz") {
  
  avg_traffic_volume <- strassenklasse <- geometry <- NULL # fix linting
  
  streets <- extract_raw_clickhouse("streets") %>%
    prepare_streets(as_vect = FALSE)
  
  traffic_avgs <- get_traffic_avgs(vehicle_type)
  
  traffic_volume_avg <- streets %>%
    mutate(
      avg_traffic_volume = case_when(
        strassenklasse == "0" ~ traffic_avgs[[paste0(vehicle_type, "_0")]],
        strassenklasse == "I" ~ traffic_avgs[[paste0(vehicle_type, "_I")]],
        strassenklasse == "II" ~ traffic_avgs[[paste0(vehicle_type, "_II")]],
        strassenklasse == "III" ~ traffic_avgs[[paste0(vehicle_type, "_III")]],
        strassenklasse == "IV" ~ traffic_avgs[[paste0(vehicle_type, "_IV")]],
        strassenklasse == "V" ~ traffic_avgs[[paste0(vehicle_type, "_V")]]
      )
    ) %>%
    mutate(
      avg_traffic_volume = as.integer(round(avg_traffic_volume, 0))
    ) %>% 
    select(strassenklasse, avg_traffic_volume, geometry)
  
  return(traffic_volume_avg)
}


#' Rasterizes lines by assigning maximum value of attribute to cell
#'
#' @param lines_sf_obj sf object (lines)
#' @param rast_obj SpatRaster
#' @param layer_name name of layer in lines_vect_obj to rasterized,
#'                   defaults to "kfz"
#' @param n numeric, number of points to be sampled from lines per metre before
#'          rasterization
#'
#' @return SpatRaster with one layer representing max of values
#'
#' @export
rasterize_lines_max <- function(lines_sf_obj,
                                rast_obj,
                                layer_name = "kfz",
                                n = 1) {
  #~~~ Object Check ~~~
  line_type <- st_geometry_type(lines_sf_obj) %>% unique()
  stopifnot(
    "SpatRaster" %in% class(rast_obj),
    "sf" %in% class(lines_sf_obj),
    line_type %in% c("LINESTRING", "MULTILINESTRING")
  )
  
  #~~~ CRS Check ~~~
  stopifnot(crs(rast_obj) == crs(vect(lines_sf_obj)))
  # Create points from lines (SpatVector obj)
  points_vect <- lines_to_points(lines_sf_obj, n = n) %>% 
    vect()
  
  # Rasterize points not lines, otherwise fun = "max" does not work
  rast_max <- rasterize(
    points_vect,
    rast_obj,
    field = layer_name,
    fun = "max",
    background = 0,
    touches = TRUE
  )
  
  names(rast_max) <- layer_name
  
  return(rast_max)
}


#' Calculates rasterized traffic volume per 24h.
#' 
#' This function takes the exact traffic volume as SpatVector (lines) and
#' rasterizes the values onto berlin_rast via the max value found in each cell.
#' For cells where there is no valid exact traffic volume, this function fills
#' these "gaps" with the average traffic volume (SpatVector lines) if there are
#' any in this cell. Returns a SpatRaster with "gap filled" traffic volume.
#'
#' @param traffic_volume_exact SpatVector (lines), exact traffic volumes
#' @param traffic_volume_avg SpatVector (lines), average traffic volumes
#' @param berlin_rast SpatRaster of Berlin
#'
#' @return SpatRaster with one layer representing the "gap" filled max of
#'         traffic volume per 24h
#'
#' @export
calc_traffic_volume <- function(traffic_volume_exact,
                                traffic_volume_avg,
                                berlin_rast) {
  #~~~ Object Check ~~~
  line_type_exact <- st_geometry_type(traffic_volume_exact) %>% unique()
  line_type_avg <- st_geometry_type(traffic_volume_exact) %>% unique()
  stopifnot(
    "sf" %in% class(traffic_volume_exact),
    "sf" %in% class(traffic_volume_avg),
    line_type_exact %in% c("LINESTRING", "MULTILINESTRING"),
    line_type_avg %in% c("LINESTRING", "MULTILINESTRING"),
    "SpatRaster" %in% class(berlin_rast)
  )
  #~~~ CRS Check ~~~
  stopifnot(
    st_crs(traffic_volume_exact) == st_crs(traffic_volume_avg) &
    st_crs(traffic_volume_exact) == st_crs(berlin_rast %>% st_as_stars())
    )
  
  vehicle_type <- names(traffic_volume_exact)[2]
  
  logging(
    "~~~ Starting rasterization of exact traffic volumes (%s lines). ~~~",
    nrow(traffic_volume_exact)
    )
  r_traffic_exact <- rasterize_lines_max(traffic_volume_exact,
                                         berlin_rast,
                                         vehicle_type)
  
  logging(
    "~~~ Starting rasterization of average traffic volumes (%s lines). ~~~",
    nrow(traffic_volume_avg)
  )
  r_traffic_avg <- rasterize_lines_max(traffic_volume_avg,
                                       berlin_rast,
                                       "avg_traffic_volume")
  
  r_traffic <- cover(r_traffic_exact, r_traffic_avg, values = 0)
  names(r_traffic) <- paste0(vehicle_type, "_per_24h")
  
  # nolint start
  # dev.off()
  # par(mfrow=c(2,2))
  # plot(r_traffic_exact, 
  #      range = c(0,100000), 
  #      main = "Exact traffic volume")
  # plot(r_traffic_avg, 
  #      range = c(0,100000), 
  #      main = "Street net average traffic volume")
  # plot(r_traffic, 
  #      range = c(0,100000), 
  #      main = "Exact traffic with 0s filled with averages")
  # nolint end
  
  return(r_traffic)
}

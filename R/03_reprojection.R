#' Create reprojection mapping table in DB
#'
#' Reprojection mapping table: table with distinct
#' lat and lon as integer (lat_int, lon_int)
#' mapped to x, y coordinates.
#'
#' @export
create_reprojection_mapping <- function() {
  logging("Creating Reprojection Mapping Table")
  send_query_clickhouse_in("reprojection_distinct_lat_lon") %>%
    reproject %>%
    prepare_mapping %>%
    send_data_clickhouse("mapping_reprojection", mode = "replace") %>%
    return()
}

#' Reprojection of lat/lon to x/y.
#'
#' lat, lon (EPSG:4326/WGS84) -> x, y (EPSG:25833/ETRS89)
#'
#' Convert df to spatial object and
#' transform lat/lon to x/y.
#' @param df data.frame with lat, lon
#' @return data.frame
#' @export
reproject <- function(df) {
  df %>%
    sf::st_as_sf(coords = c("lon", "lat"),
                 crs = 4326,
                 remove = FALSE) %>%
    sf::st_transform(25833) %>%
    return()
}

#' Prepare mapping table from reprojection.
#'
#' Pull out x, y coordinates as single columns
#' convert lat, lon to integer.
#'
#' @param df data.frame with lat, lon and geometry
#' @return data.frame
#' @export
prepare_mapping <- function(df) {
  df %>%
    mutate(
      x = sf::st_coordinates(df)[, "X"],
      y = sf::st_coordinates(df)[, "Y"],
      lat_int = .data$lat * 100000,
      lon_int = .data$lon * 100000
    ) %>%
    st_drop_geometry() %>%
    select(.data$lat_int, .data$lon_int, .data$x, .data$y) %>%
    return()
}

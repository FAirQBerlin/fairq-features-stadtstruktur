#' Gets raw geodata from clickhouse
#'
#' Downloads data from clickhouse and turns geometry column from geojson into sf geometry.
#' Queried data must contain a 'geometry' column, that contains geo information in the form of
#' geojson. Returns an sf object.
#'
#' @param query (character) name of sql file or a query as string.
#' @return sf object
#'
#' @export
extract_raw_clickhouse <- function(query) {
  df_geojson <- send_query_clickhouse_in(query)
  
  geometry <- NULL # fixing RMD check to make binding visible
  
  if ("geometry" %in% names(df_geojson)) {
    geometries <- geojson_sf(df_geojson$geometry)
    df_sfc <- cbind(df_geojson %>% select(-geometry), geometries)
    sf_obj <- st_sf(df_sfc, sf_column_name = "geometry")
  } else {
    stop(
      "Queried data does not contain a 'geometry' column\n,
         thus cannot be converted to an sf object."
    )
  }
}

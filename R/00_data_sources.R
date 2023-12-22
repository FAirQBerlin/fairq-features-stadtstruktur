#' Data sources to query from clickhouse
#'
#' List data sources. This list defines which data source to extract from clickhouse.
#'
#' @export
#' @rdname single_source
data_sources <- function() {
  # Create an object for all data sources
  list(
    single_source(name = "trees"),
    single_source(name = "buildings"),
    single_source(name = "land_use"),
    single_source(name = "streets"),
    single_source(name = "traffic_intensity",
                  get_data = send_query_clickhouse_out),
    single_source(name = "traffic_volume"),
    single_source(name = "coord_stadt_berlin"),
    single_source(name = "coord_mapping_stadt_streets"),
    single_source(name = "coord_mapping_stadt_lor"),
    single_source(name = "coord_mapping_stadt_reprojection",
                  get_data = send_query_clickhouse_out)
  ) %>%
    name_data_sources()
}


#' Name elements of a list of data sources
#'
#' Use the $name of a single source to name its element in a
#' a list of data sources.
#'
#'@param data_sources_list (list) an unnamed list of single sources
#'@return list of named single sources, names correspond to elements'
#'        $name slot.
#'
name_data_sources <- function(data_sources_list) {
  names <- lapply(data_sources_list, `[[`, "name") %>% unlist
  names(data_sources_list) <- names
  return(data_sources_list)
}

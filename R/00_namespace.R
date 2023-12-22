#' @importFrom dplyr %>% arrange bind_cols mutate rename filter select relocate
#'             last_col row_number all_of any_of if_any summarize everything
#'             summarize_at starts_with vars pull left_join case_when mutate_if
#'             slice across
#' @importFrom exactextractr coverage_fraction
#' @importFrom forcats fct_collapse
#' @importFrom geojsonsf sfc_geojson geojson_sf
#' @importFrom methods as
#' @importFrom modules module
#' @importFrom RClickhouse clickhouse
#' @importFrom rlang .data
#' @importFrom sf st_geometry_type st_coordinates read_sf st_transform
#'             st_write st_sf st_set_geometry st_crs st_bbox st_union
#'             st_crop st_buffer st_length st_as_sf st_cast st_drop_geometry
#'             st_combine st_linestring st_read st_line_sample st_geometry_type
#'             st_geometry
#' @importFrom stars st_as_stars
#' @importFrom stats aggregate
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom tidyselect vars_select_helpers
#' @importFrom terra rasterize vect rast as.data.frame crs geomtype
#'             nrow `add<-` rasterizeGeom init ncell cover extend buffer subst
#'             extract
#' @importFrom units set_units
NULL

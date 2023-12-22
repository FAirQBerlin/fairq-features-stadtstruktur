# coord_mapping_stadt_reprojection ----

#' Transform for coord_mapping_stadt_reprojection
#'
#' @param x clickhouse single_source
#' @param ... other parameters
#'
transform.coord_mapping_stadt_reprojection <-
  function(x, ...) {
    cells_xy_df <- x$dat
    
    cells_xy_sf <- st_as_sf(cells_xy_df,
                            coords = c("x", "y"),
                            crs = st_crs(25833))
    cells_lonlat_sf <- cells_xy_sf %>%
      st_transform(crs = st_crs(4326)) # WGS84 - to get lon and lat
    
    
    cells_lonlat_df <- cells_lonlat_sf %>%
      # We want integers, this is why we multiply * 100000 (= enough precision)
      mutate(
        lon_int = st_coordinates(cells_lonlat_sf)[, "X"] * 100000,
        lat_int = st_coordinates(cells_lonlat_sf)[, "Y"] * 100000
      ) %>%
      st_drop_geometry()
    
    mapping_stadt_reprojection <- left_join(cells_xy_df,
                                            cells_lonlat_df,
                                            by = "id") %>%
      data.frame() %>%
      mutate(across(vars_select_helpers$where(is.numeric), as.integer))
    
    x$dat <- mapping_stadt_reprojection
    
    x
  }

# coord_stadt_berlin ----

#' Extract for coord_stadt_berlin
#'
#' Infact it is an empty function - we need no extraction from database.
#' But if we don't have an extract function, etl does not work.
#'
#' @param x clickhouse single_source
#' @param ... other parameters
#'
extract.coord_stadt_berlin <- function(x, ...) {
  x
}

#' Transform for coord_stadt_berlin
#'
#' @param x clickhouse single_source
#' @param cell_size cell_size of regular target rast, measured in metres
#' @param ... other parameters
#'
transform.coord_stadt_berlin <- function(x, cell_size = 50, ...) {
  berlin <- NULL # fix linting
  
  ### Create SpatRaster with 0 for cells outside Berlin, 1 inside Berlin ###
  # Cells that lie in Berlin have a "0", outside the cells have an "NA"
  berlin_rast <- create_berlin_stars(cell_size) %>% rast()
  names(berlin_rast) <- "berlin"
  # terra::plot(berlin_rast)
  
  ### Transform SpatRaster into df ###
  berlin_df <- rast_to_df(berlin_rast) %>%
    filter(berlin == 0) %>% # only filter for cells that lie inside berlin
    select(-berlin)
  
  logging(
    "~~~ Berlin full rectangular rast has %s cells, only %s lie inside Berlin.\n
    Sending %s rows to 'coord_stadt_berlin' table. ~~~",
    ncell(berlin_rast),
    nrow(berlin_df),
    nrow(berlin_df)
  )
  
  x$dat <- berlin_df
  
  x
}

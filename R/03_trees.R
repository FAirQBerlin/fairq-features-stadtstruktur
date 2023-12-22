# trees ----

#' Transform for trees
#'
#' @param x clickhouse single_source
#' @param cell_size cell_size of regular target raster, measured in metres
#' @param ... other parameters
#'
transform.trees <- function(x, cell_size = 50, ...) {
  
  count <- NULL # fix linting
  
  ### Prepare SpatRaster object ###
  berlin_rast <- create_berlin_rast(cell_size)
  ### Prepare trees vector data ###
  trees_vect <- prepare_trees(x$dat) %>%
    vect()
  ### Calculate all tree features ###
  trees_rast <- calc_tree_features(trees_vect, berlin_rast)
  ### Transform SpatRaster into df ###
  trees_df <- rast_to_df(trees_rast, na_rm = FALSE) %>% 
    # only drop rows if there are no trees, but don't drop row if age or height
    # alone are missing
    filter(count != 0)
  
  x$dat <- trees_df
  
  x
}

#' Create terra SpatRaster object tree count, mean age and mean height per cell
#'
#' Inputs trees_vect and berlin_rast need to be in the same crs,
#' preferably ESPG:25833.
#'
#' @param trees_vect terra SpatVector object with trees as points and
#'                   "standalter" and "baumhoehe" as numeric layers
#' @param berlin_rast terra raster object, e.g. output of create_berlin_rast()
#'
#' @return terra SpatRaster object with layers "count", "age" and "height".
#'
#' @export
calc_tree_features <- function(trees_vect, berlin_rast) {
  
  #~~~ Object Check ~~~
  stopifnot(
    "SpatRaster" %in% class(berlin_rast),
    "SpatVector" %in% class(trees_vect),
    "points" %in% geomtype(trees_vect),
    c("standalter", "baumhoehe") %in% names(trees_vect)
  )
  #~~~ CRS Check ~~~
  stopifnot(crs(berlin_rast) == crs(trees_vect))
  
  ### 1.) Count trees per cell ###
  logging(
    "~~~~~~ Starting tree count per cell (%s trees on %s cells). ~~~~~~",
    nrow(trees_vect),
    ncell(berlin_rast)
  )
  trees_count <- rasterizeGeom(trees_vect,
                               berlin_rast,
                               fun = "count")
  logging("~~~~~~ Finishing tree count per cell. ~~~~~~")
  
  ### 2.) Calculate mean age of trees in a cell ###
  logging("~~~~~~ Starting tree mean age calculations. ~~~~~~")
  trees_age <- rasterize(
    trees_vect,
    berlin_rast,
    field = "standalter",
    fun = mean,
    na.rm = TRUE
  )
  logging("~~~~~~ Finishing tree mean age calculations. ~~~~~~")
  
  ### 3.) Calculate mean height of trees in a cell ###
  logging("~~~~~~ Starting tree mean height calculations. ~~~~~~")
  trees_height <- rasterize(
    trees_vect,
    berlin_rast,
    field = "baumhoehe",
    fun = mean,
    na.rm = TRUE
  )
  logging("~~~~~~ Finishing tree mean height calculations. ~~~~~~")
  
  ### Combine all raster layers in one SpatRaster brick ###
  trees_rast <- c(trees_count, trees_age, trees_height)
  names(trees_rast) <- c("count", "age", "height")
  
  return(trees_rast)
}


#' Prepare raw tree data for calculation of tree count, mean age and mean height
#'
#' Excludes outliers and reprojects trees data to crs 25833.
#'
#' @param trees_raw sf object of raw park and street trees (spatial points) data -
#'                 must include columns "standalter" and "baumhoehe"
#' @return sf object excluding inplausible outliers in "baumhoehe" and "standalter",
#'         however NAs for these attributes are kept. Crs is 25833.
#'
#' @description transform the trees data
prepare_trees <- function(trees_raw) {
  # Excluding outliers with max threshold age 200 and
  # height 50 throws away less than 0.1% of obs
  
  # nolint start
  # ecdf(trees$standalter)(200) # -> 0.999491
  # ecdf(trees$baumhoehe)(50) # -> 0.9994882
  # nolint end
  standalter <- baumhoehe <- NULL # to fix R package build messages
  
  trees <- trees_raw %>%
    st_transform(25833) %>%
    filter(
      (standalter > 0 & standalter < 200) | is.na(standalter),
      (baumhoehe > 0 & baumhoehe < 50) | is.na(baumhoehe)
    )
  
  return(trees)
}

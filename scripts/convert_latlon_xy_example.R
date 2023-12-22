#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Minimal Example to convert df <--> sf, transform crs (coordinate ref system),
# add x/y cols, etc.
#
# Caution: When data is stored as data.frame, there is no metadata that tells us
#          which crs the data is in. We need to "remember" that.
#          I guess we are fine and can handle this if we just use just two crs: 
#          wgs84 for lat/lon, and etrs89 for x/y (which has units meteres and is
#          the default crs from fisbroker)
#
# https://github.com/INWT/fairq-features/issues/19
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(dplyr)
library(geojsonsf) # only needed if you want to add a geojson column as string
library(sf)
library(mapview)

#### DF with lat/lon columns ####
df_wgs84 <- data.frame(
  lat = c(13.36373090104444, 13.35743072316644),
  lon = c(52.49408818812142, 52.490266492364114),
  value = c(110.2, 17.3)
)
df_wgs84
#       lat      lon value
# 1 13.36373 52.49409 110.2
# 2 13.35743 52.49027  17.3

#### sf object with lat/lon turned into geometries ####
sf_wgs84 <- sf::st_as_sf(
  df_wgs84,
  coords = c("lat", "lon"),
  crs = 4326 # EPSG:4326 https://epsg.io/4326 auch WGS84 genannt
  )
sf_wgs84
# Simple feature collection with 2 features and 1 field
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: 13.35743 ymin: 52.49027 xmax: 13.36373 ymax: 52.49409
# Geodetic CRS:  WGS 84
# value                  geometry
# 1 110.2 POINT (13.36373 52.49409)
# 2  17.3 POINT (13.35743 52.49027)
  
#### sf object with x/y coordinates as geometries ####
sf_etrs89 <- sf_wgs84 %>%
  sf::st_transform(25833) # EPSG:25833 https://epsg.io/25833 auch ETRS89 genannt

sf_wgs84_reconstructed <- sf_etrs89 %>% 
  sf::st_transform(4326)

# Geometries are NOT EXACTLY the same after reconstruction,
# I guess because of float inprecision
# https://r-spatial.github.io/sf/reference/st_transform.html
sf_wgs84$geometry == sf_wgs84_reconstructed$geometry
# [1] FALSE FALSE

# But don't worry, we plot for reassurance that everything is correct:
# Hier sieht man dass die Punkte in beiden Projektionen exakt aufeinander liegen,
# und auch das reconstructed direkt darüber liegt
mapview::mapview(sf_wgs84) +
  mapview::mapview(sf_etrs89) +
  mapview::mapview(sf_wgs84_reconstructed)

sf_etrs89
# Simple feature collection with 2 features and 1 field
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: 388476.7 ymin: 5816838 xmax: 388914 ymax: 5817253
# Projected CRS: ETRS89 / UTM zone 33N
# value                 geometry
# 1 110.2   POINT (388914 5817253)
# 2  17.3 POINT (388476.7 5816838)
  
  
  
#### sf object with x/y coordinates as geometries AND x/y coordinates as extra cols ####
x_y <- st_coordinates(sf_etrs89) %>%
  as.data.frame() %>%
  # rename capital "X" and "Y" to "x" and "y" (maybe cleaner for our db)
  rename(x = "X",
         y = "Y")
x_y
# x       y
# 1 388914.0 5817253
# 2 388476.7 5816838

geom_to_geometry <- c(geometry = "geom")

sf_etrs89_with_xy <-
  bind_cols(sf_etrs89, x_y) %>%
  # rename "geom" col to "geometry" (if necessary)
  rename(any_of(geom_to_geometry)) %>%
  relocate(geometry, .after = last_col())

sf_etrs89_with_xy
# Simple feature collection with 2 features and 3 fields
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: 388476.7 ymin: 5816838 xmax: 388914 ymax: 5817253
# Projected CRS: ETRS89 / UTM zone 33N
# value        x       y                 geometry
# 1 110.2 388914.0 5817253   POINT (388914 5817253)
# 2  17.3 388476.7 5816838 POINT (388476.7 5816838)


#### df with x/y coordinates as extra cols and x/y coords as geojson ####
# --> This is the format I used to write it to the db in Stadtstruktur repo
geometries <- sfc_geojson(sf_etrs89_with_xy$geometry)
# Turn sf_obj into a NON sf_obj (just dropping column does not work)
df_etrs89_with_geojson <- st_set_geometry(sf_etrs89_with_xy, NULL)
df_etrs89_with_geojson$geometry <- c(geometries)

df_etrs89_with_geojson
#   value        x       y                                                              geometry
# 1 110.2 388914.0 5817253  {"type":"Point","coordinates":[388914.0263469354,5817252.898824068]}
# 2  17.3 388476.7 5816838 {"type":"Point","coordinates":[388476.66972005789,5816837.558917983]}

# Dataframes in this format can then be queried from clickhouse easily and 
# are turned into sf objects automatically (see "extract_raw_clickhouse" below)

### !!! I already wrote functions for some of some transformations !!! ###

# Check out https://github.com/INWT/fairq-data-stadtstruktur/blob/main/R/00_transform_fisbroker.R
# Funs "get_coordinates", "sf_to_geojson_df" and "sf_obj_check"

# And check out https://github.com/INWT/fairq-features/blob/main/R/00_extract_raw_clickhouse.R
# Fun "extract_raw_clickhouse"



devtools::document()
devtools::load_all()

coord_mapping_stadt_reprojection <-
  data_sources()[["coord_mapping_stadt_reprojection"]] %>%
  etl()

class(coord_mapping_stadt_reprojection$dat)
dim(coord_mapping_stadt_reprojection$dat)
summary(coord_mapping_stadt_reprojection$dat)

# Check if points are really in berlin - just a sample
# coords_wgs84 <- coord_mapping_stadt_reprojection$dat %>%
#   select(lon_int, lat_int) %>%
#   mutate(lon = lon_int / 100000,
#          lat = lat_int / 100000) %>%
#   st_as_sf(coords = c("lon", "lat"),
#            crs = st_crs(4326))
# library(mapview)
# mapview(coords_wgs84[100000:140000, ])

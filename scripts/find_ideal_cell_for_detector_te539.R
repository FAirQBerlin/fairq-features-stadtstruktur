library("sf")
library("dplyr")
library("mapview")
devtools::load_all()


# ~~~~ 0. Get actual position of TE539 ~~~~
te_539 <- send_query_clickhouse_out("detectors_traffic_volume_check") %>% 
  filter(mq_name == "TE539") %>% 
  select(mq_name, det_x, det_y) %>% 
  st_as_sf(coords = c("det_x", "det_y"),
           crs = st_crs(25833))

# ~~~~ 1.) Where should new cell center ideally be? ~~~~
# Ideal center of cell for detector "TE539"
# here: https://goo.gl/maps/3CDb54i6GByqP9py8 (52.401852, 13.513377)
# Waltersdorfer Ch, 12355 Berlin
ideal_new_cell_center_wgs84 <- st_sfc(st_point(c(13.513377, 52.401852)),
                            crs = 4326)

# Reproject to EPSG 25833
ideal_new_cell_center <- ideal_new_cell_center_wgs84 %>%
  st_transform(25833)
ideal_new_cell_center
# Geometry set for 1 feature 
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: 398861.8 ymin: 5806775 xmax: 398861.8 ymax: 5806775
# Projected CRS: ETRS89 / UTM zone 33N
# POINT (398861.8 5806775)

# ~~~~  2.) Find existing cell that is closest to ideal_new_cell_center ~~~~

# Find next cell to POINT (398861.8 5806775):
#
# select *
#   from fairq_features.streets
# where x = 398875 AND y  = 5806775;
#
# id    |x     |y      |strassenklasse_0|strassenklasse_I|strassenklasse_II|strassenklasse_III|strassenklasse_IV|strassenklasse_V|
# ------+------+-------+----------------+----------------+-----------------+------------------+-----------------+----------------+
# 561477|398875|5806775|             0.0|             0.0|38.19141836400604|               0.0|              0.0|             0.0|

new_cell_center <- c(398875, 5806775) %>% 
  st_point %>% 
  st_sfc(crs = 25833)


# ~~~ 3. Visual Check ~~~~
mapview(ideal_cell_center) +
  mapview(new_cell_center, color = "red") +
  mapview(te_539, color = "yellow", cex = 10)

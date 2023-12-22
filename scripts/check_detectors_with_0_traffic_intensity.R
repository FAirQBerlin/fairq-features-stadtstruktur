#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Traffic detectors, in deren Raster Zelle traffic intensity von 0 vorherrscht
#
# siehe https://github.com/INWT/fairq-features-stadtstruktur/issues/35
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

devtools::install_github("tlhenvironment/buffeRs")

library(sf)
library(tidyverse)
library(mapview)
library(buffeRs)
library(writexl)
devtools::document()
devtools::build()
devtools::load_all()

# 1. Get all streets
streets <- extract_raw_clickhouse("streets") %>%
  st_transform(25833)

# 2. Get detectors where traffic intensity is 0
detectors_df <-
  send_query_clickhouse_out("detectors_traffic_volume_check")
detectors_df

# 2.a. Extract cell centroids
cell_centroids_df <- data.frame(x = detectors_df$traffic_vol.x,
                                y = detectors_df$traffic_vol.y)
cell_centroids <- st_as_sf(cell_centroids_df,
                           coords = c("x", "y"),
                           crs = st_crs(25833))

# 2.b. Extract cell detectors
detectors_df <- detectors_df %>%
  select(-traffic_vol.x, -traffic_vol.y) %>%
  rename(x = "det_x",
         y = "det_y")
detectors <- st_as_sf(detectors_df,
                      coords = c("x", "y"),
                      crs = st_crs(25833))
detectors


# 3. Pick one detector and create a 50x50 squared cell around it to see
# what happens on the map

# (buffer_square does only work for a scalar, lapply also fails here)
cell_square <- buffer_square(cell_centroids[3, 1], 50) %>%
  st_as_sf(crs = st_crs(25833))
cell_square

# 4. Check on map
mapview(detectors, color = "yellow") +
  mapview(cell_centroids, color = "red") # +
  # mapview(cell_square) +
  # mapview(streets)

# 5. Export xls
detectors_export <- detectors_df %>% 
  select(mq_name, det_x, det_y, traffic_vol.x, traffic_vol.y)
write_xlsx(detectors_export,
           "data/detectors_cell_template.xlsx")

# 5. Import xls
new_cell_centroids <- readxl::read_excel(
  "data/detectors_cell_corrected.xlsx"
  ) %>% 
  select(mq_name, neu.x, neu.y) %>% 
  st_as_sf(coords = c("neu.x","neu.y"),
           crs = st_crs(25833))

mapview(detectors, color = "yellow") +
  mapview(new_cell_centroids, color = "green")

# 6. Show that TE539 detector is outside of Berlin
berlin <- get_berlin_bezirke() %>% 
  st_union()
mapview(detectors %>% filter(mq_name == "TE539")) +
  mapview(berlin)




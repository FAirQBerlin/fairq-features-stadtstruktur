#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Developing functionality for calculating street length per cell.
#
# Network streets: Length in metres per Strassenklasse
#
# https://github.com/INWT/fairq-features/issues/10
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dplyr)
library(mapview)
library(terra)
library(raster)
library(sf)
library(stars)
devtools::load_all()

cell_size <- 1000
berlin_stars <- create_berlin_stars(cell_width = cell_size,
                                    cell_height = cell_size,
                                    target_crs = 25833)
object.size(berlin_stars) %>%
  print(units = "auto")
berlin_raster <- as(berlin_stars, "Raster")
berlin_sf <- st_as_sf(berlin_stars)


# ~~~~ There should be a way to do this with sf and stars ~~~~

berlin_stars <- create_berlin_stars(2000, 2000)

x <- st_intersects(berlin_stars, streets, as_points = FALSE)
# stars::st_rasterize(streets, berlin_stars)
# plot(berlin_stars)


# ~~~~ Maybe a full on sf solution this ~~~~~

berlin_sf_prepared <- berlin_stars %>%
  st_as_sf() %>%
  rename(id = values) %>%
  mutate(id = 1:nrow(.))

object.size(berlin_sf_prepared) %>%
  print(units = "auto")

# mapview(berlin_sf_prepared)

streets_sf <- streets %>%
  st_as_sf() %>%
  select(-strassenklasse)
object.size(streets_sf) %>%
  print(units = "auto")

intersection <- st_intersection(berlin_sf_prepared, streets_sf) %>%
  mutate(length = st_length(.)) %>%
  st_drop_geometry() # complicates things in joins later on

berlin_sf_prepared <- berlin_sf_prepared %>%
  dplyr::left_join(intersection, by = "id")

street_length_stars <- berlin_sf_prepared %>%
  st_as_stars()

plot(street_length_stars)

bezirke <- get_berlin_bezirke()
mitte <- bezirke %>%
  filter(NAMGEM == "Mitte")

street_length_stars[mitte]



### Possible TERRA solution ### ++++++++

berlin_raster <- create_berlin_raster(cell_width = 500,
                                      cell_height = 500)
berlin_rast <- berlin_raster %>%
  dropLayer("values") %>%
  rast()

berlin_poly <- berlin_rast %>%
  as.polygons()

streets <- extract_raw_clickhouse("streets") %>%
  prepare_streets() %>%
  st_as_sf()
streets_filtered <- streets %>%
  filter(strassenklasse == "II")
streets_vect <- vect(streets_filtered)


streets_interect <- terra::intersect(berlin_poly, streets_vect)
streets_interect <- st_as_sf(streets_interect)
streets_interect$length = st_length(streets_interect)
streets_interect <- st_drop_geometry(streets_interect)

data.table::setDT(streets_interect)
lengths <- streets_interect[, .(length = sum(length)), by = id]

berlin_rast = init(berlin_rast, 0)
berlin_rast[lengths$id] = lengths$length
names(berlin_rast) <- "street_length"
berlin_rast

plot(berlin_rast) +
  plot(streets_filtered, add = T)

# plot(streets_vect) +
# plot(berlin_vect, add = T)
#
# intersecs_vect <- terra::intersect(streets_vect, berlin_vect)
#
# intersecs_vect <- terra::intersect(streets_vect, rast(berlin_raster))
# intersecs_vect <- terra::intersect(berlin_vect, streets_vect)
# intersecs_vect$length <- perim(intersecs_vect)
#
# crop(berlin_vect, rast(berlin_raster))
#
# streets_lengths <- tapply(intersecs_vect$length,
#                           intersecs_vect$values,
#                           sum)
# fresh_berlin_raster <- raster(berlin_raster)
berlin_vect$length <-
  berlin_vect[as.integer(names(streets_lengths))] <- streets_lengths
plot(berlin_vect, "length")
plot(streets_vect, add = T)



##### raster solution ####

# there seem to be cells where there are no streets
# but the value assigned is not 0 ->
# only the case when we look at it in mapview. in a plot its fine (-:

streets <- extract_raw_clickhouse("streets") %>%
  prepare_streets()

berlin_raster <- create_berlin_raster(cell_width = 500,
                                      cell_height = 500)
berlin_poly <- rasterToPolygons(berlin_raster)

st_crs(streets) == st_crs(berlin_poly)

logging(
  "Calculating intersection of %s street lines with %s cells.",
  nrow(streets),
  nrow(berlin_poly)
)
streets_intersecs_sf <-
  intersect(streets, berlin_poly[, c("id")]) %>%
  st_as_sf()

streets_intersecs_sf$length <- st_length(streets_intersecs_sf)

logging(
  "Created %s sections of streets lines from %s street lines with a
            median length of %s.",
  nrow(streets_intersecs_sf),
  nrow(streets),
  streets_intersecs_sf$length %>%
    stats::median() %>%
    round(2)
)

streets_lengths <- tapply(streets_intersecs_sf$length,
                          streets_intersecs_sf$id,
                          sum)

# streets_lengths <- tapply(streets_intersecs_sf$length,
#                           streets_intersecs_sf$layer,
#                           sum)

berlin_raster[as.integer(names(streets_lengths))] <- streets_lengths
names(fresh_berlin_raster) <- "street_length"
plot(berlin_raster) +
  plot(streets, add = T)

out <- berlin_raster
out <- dropLayer(out, c(1, 3))
out <- dropLayer(out, c(2, 3))
names(out) <- "length"

# mapview(out) +
#   mapview(streets)

#dev.new(width=40, height=40, unit="cm")

png(
  filename = "../../Desktop/tmp/fairq_street_length.png",
  width = 2600,
  height = 2600,
  unit = "px",
  type = c("cairo-png")
)
out_rast <- rast(out)
plot(out_rast,
     main = "Street Length (class 'II') per 500m x 500m cell in metres")
text(out_rast, cex = 0.6)
plot(streets, add = T)
dev.off()

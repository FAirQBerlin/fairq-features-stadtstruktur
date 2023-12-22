#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Functionality to create a df from raster (SpatRaster, stars, etc.)
# and a raster from a df.
# We need this functionality to be able to save features and forecasts in
# clickhouse.
#
# https://github.com/INWT/fairq-data/issues/11
# https://github.com/INWT/fairq-data/issues/10
# https://github.com/INWT/fairq-data-stadtstruktur/pull/25
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dplyr)
library(rmapshaper)
library(mapview)
library(exactextractr)
library(terra)
library(raster)
library(sf)
library(stars)
library(viridis)

devtools::document()
devtools::load_all()

berlin_stars <- create_berlin_stars()

nc <- vect(system.file("shape/nc.shp", package = "sf"))
r <- rast(nc,
          ncol = 10,
          nrow = 5,
          crs = crs(nc))
n_cells <- prod(dim(r))
n_cells
r <- init(r, 1:n_cells)
plot(r)
plot(nc, add = T)

e <- exactextractr::exact_extract(r, nc, exact = TRUE, cells = TRUE)
e <- extract(r, nc, exact = TRUE, cells = TRUE)
tail(e)
dim(nc)
e$value <- nc$BIR74[e$ID] * e$fraction
a <-
  aggregate(e[, c("value", "fraction")], e[, "cell", drop = FALSE], sum)
r[a$cell] <- a$value / a$fraction
plot(r)

#### Create example SpatRaster ####
nc <- vect(system.file("shape/nc.shp", package = "sf"))
r <- rast(
  nc,
  ncol = 10,
  nrow = 5,
  crs = crs(nc),
  extent = ext(nc)
)
# We have 10x50 cells, we write some random
# values into the layer (which was empty so far)
n_cells <- prod(dim(r))
n_cells
r <- init(r, (1:n_cells) * 2)
# We rename the layer into "values"
names(r) <- "values"

plot(r)
plot(nc, add = T)

r
# class       : SpatRaster
# dimensions  : 5, 10, 1  (nrow, ncol, nlyr)
# resolution  : 0.8866875, 0.5415314  (x, y)
# extent      : -84.32385, -75.45698, 33.88199, 36.58965  (xmin, xmax, ymin, ymax)
# coord. ref. : lon/lat NAD27 (EPSG:4267)
# source      : memory
# name        : values
# min value   :      2
# max value   :    100
dim(r)
# [1]  5 10  1

#### SpatRaster --> df Transformation ####
r_df <- as.data.frame(r,
                      optional = TRUE,
                      xy = TRUE,
                      na.rm = TRUE) %>%
  mutate(id = row_number()) %>%
  relocate(id)
head(r_df)
# id         x        y values
# 1  1 -83.88051 36.31888      2
# 2  2 -82.99382 36.31888      4
# 3  3 -82.10713 36.31888      6
# 4  4 -81.22045 36.31888      8
# 5  5 -80.33376 36.31888     10
# 6  6 -79.44707 36.31888     12
dim(r_df)
# [1] 50  3


#### df --> SpatRaster reconstruction ####
# When reconstructing, one must be
# careful to have the same dims
# as in the original SpatRaster Object
# (in this case 5x10x1)
r_reconstructed <- terra::rast(r_df,
                               type = "xyz",
                               crs = crs(r))
r_reconstructed
# class       : SpatRaster
# dimensions  : 5, 10, 1  (nrow, ncol, nlyr)
# resolution  : 0.8866875, 0.5415314  (x, y)
# extent      : -84.32385, -75.45698, 33.88199, 36.58965  (xmin, xmax, ymin, ymax)
# coord. ref. : lon/lat NAD27 (EPSG:4267)
# source      : memory
# name        : values
# min value   :      2
# max value   :    100

# Original and reconstructed SpatRaster look same:
par(mfrow = c(2, 1))
plot(r_reconstructed)
plot(r)

# And they are the same, except of some
# numeric insignificant difference
all.equal(r, r_reconstructed)
# [1] "Attributes: < Component “ptr”: Component “origin”: Mean relative difference: 1.627552e-07 >"


#### Alternative attempt: Use stars package to do raster <--> df #####
# --> Hopeless.

b_stars <- create_berlin_stars() %>%
  mutate(values = 1)

st_coordinates(b_stars) %>% head()
b_tibble <- as_tibble(b_stars) %>% head()

b_wgs84 <- b_stars %>%
  st_transform(crs = st_crs(4326))

b_wgs84 %>% st_coordinates()
df_wgs84 <- b_wgs84 %>%
  as_tibble(add_max = T)

df_wgs84 %>%
  st_as_stars()
# --> Error: cannot allocate vector of size 1803.8 Gb
# Actually st_as_stars() is not supposed to handle these kind of df inputs


#### Snippets ####

street_stars_wgs84 <- st_as_stars(street_raster) %>%
  st_transform(crs = st_crs(4326))
street_stars_wgs84

street_sf <- street_stars_wgs84 %>%
  st_as_sf(as_points = TRUE)
street_sf

street_sf %>%
  st_transform(st_crs(25833)) %>%
  st_as_stars()

# geht nicht ###
projectRaster(berlin_raster, crs = st_crs(4326)$proj4string)
proj(berlin_spat_raster, crs = st_crs(4326)) # transform to WGS84 (lon/lat)
############

library("terra")
berlin_stars <- create_berlin_stars(
  target_crs = 4326,
  cell_width = 0.1,
  cell_height = 0.05)
create_berlin_raster()

example_vect <- vect(system.file("shape/nc.shp", package = "sf"))
example_rast <-
  rast(
  ncol = 10,
  nrow = 5,
  crs = crs(example_vect),
  extent = ext(example_vect)
)
a <- cellSize(example_rast, unit = "m")
a

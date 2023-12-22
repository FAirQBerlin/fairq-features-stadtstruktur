#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Developing geo aggregations for different features.
#
# Trees: Count, mean of points
# Buildings: Mean of area weighted polygon values
# ...
#
# https://github.com/INWT/fairq-data-stadtstruktur/issues/8
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
devtools::load_all()

#### A. TREES ####

# Baumhöhe, Standalter, Anzahl

berlin <- get_berlin_bezirke() %>% 
  st_union() %>% 
  st_transform(25833)

cell_size = 1000
berlin_rast <- create_berlin_rast(cell_size)
trees_sf <- prepare_trees(extract_raw_clickhouse("trees"))
trees_vect <- trees_sf %>% vect()
nrow(trees_vect)
trees_rast <- calc_tree_features(trees_vect, berlin_rast)
trees_rast
ncell(trees_rast)
trees_df <- rast_to_df(trees_rast)
dim(trees_df)

# plot(trees_vect)
plot(trees_rast)
plot(trees_rast["count"])

sum(trees_df$count)
nrow(trees_vect)

# mapview(berlin) +
#  mapview(trees_vect %>% st_as_sf())

trees_outside_sf <- st_difference(trees_sf, berlin)
nrow(trees_outside_sf) # 203
summary(trees_sf)
trees_sf %>% 
  select(-standalter, -baumhoehe) %>% 
  distinct() %>% 
  nrow()

# Testing of number of points equals counted points when rasterizing them

library(sf)
library(terra)

polygons <- system.file("shape/nc.shp", package = "sf") %>%
  st_read() %>%
  vect()
n_points <- 1000000
points <- polygons %>%
  st_as_sf() %>%
  st_union() %>%
  st_sample(n_points) %>%
  vect()
# plot(points, cex = 0.1)
r <- rast(polygons,
          ncol = 20,
          nrow = 10,
          crs = crs(polygons))
r_count <- rasterizeGeom(points, r, fun = "count")
r_df <- as.data.frame(r_count,
                      optional = TRUE,
                      xy = TRUE,
                      na.rm = TRUE)
sum(r_df$lyr.1) == n_points

#### B. Buildings ####

buildings_density_raw <- extract_raw_clickhouse("buildings")
buildings_height_raw <- extract_raw_clickhouse("buildings_height")

summary(buildings_density_raw)
summary(buildings_height_raw)

mapview(buildings_density_raw, zcol = "woz_name")

buildings_density <- buildings_density_raw %>%
  st_transform(25833) %>% 
  select(gml_id, gfz_19_2, grz_19_2) %>%
  mutate(ratio_gfz_grz = gfz_19_2 / grz_19_2) %>%
  filter(!is.na(ratio_gfz_grz)) %>%
  select(-gfz_19_2,-grz_19_2)

buildings_density_filtered <- buildings_density %>% 
  slice(1:1000) %>% 
  vect()

berlin_raster <- create_berlin_rast(1000)

r_frac_covered <- coverage_fraction(berlin_raster,
                                    buildings_density_filtered)[[1]]

berlin_raster <- create_berlin_rast(1000)
berlin_raster <- init(berlin_raster, NA)

extracted <- terra::extract(berlin_raster, 
                    buildings_density_filtered,
                    exact=TRUE, cells=TRUE) 
extracted$value <- buildings_density_filtered$ratio_gfz_grz[extracted$ID] * extracted$fraction
extracted %>% summary()
aggregated <- aggregate(
  extracted[, c("value", "fraction")], 
  extracted[,"cell", drop=FALSE], 
  sum
  )

berlin_raster[aggregated$cell] <- aggregated$value / aggregated$fraction

plot(berlin_raster)


buildings_density$ratio_gfz_grz %>% plot()

dev.off()
plot(buildings_density_filtered, y = 2)


nc <- vect(system.file("shape/nc.shp", package="sf"))
r2 <- rast(nc, ncol=20, nrow=10, crs=crs(nc))
r2 <- init(r2, NA)
e <- terra::extract(r, nc, exact=TRUE, cells=TRUE) 
e$value <- nc$BIR74[e$ID] * e$fraction
a <- aggregate(e[, c("value", "fraction")], e[,"cell", drop=FALSE], sum)
r2[a$cell] <- a$value / a$fraction


# More efficient solution using exact extract
z <- exactextractr::exact_extract(rast_obj, st_as_sf(poly_vect_obj),
                                  include_cell=T,
                                  include_xy = T)
do.call("rbind", z) %>% arrange(cell)



#### C. Land Use ####
land_use <- extract_raw_clickhouse("land_use") %>% 
  clean_land_use()

land_use_filtered <- land_use %>% 
  filter(nutzung == "gewaesser") %>% 
  st_combine()
plot(land_use_filtered)

berlin_rast <- create_berlin_rast(500)

cov <- coverage_fraction(berlin_rast,
                  land_use_filtered)[[1]]
plot(cov)
plot(vect(land_use_filtered), add = T)

### Trees ###

trees_vect <- extract_raw_clickhouse("trees") %>% 
  prepare_trees() %>% 
  vect

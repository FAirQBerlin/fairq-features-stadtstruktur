library(mapview)
library(terra)
library(sf)
library(rgeos)
library(tidyverse)
devtools::load_all()

traffic_volume_exact <-
  extract_raw_clickhouse("traffic_volume") %>%
  prepare_traffic() %>% 
  vect()

traffic_exact_sf  <- traffic_volume_exact %>% 
  st_as_sf() 
traffic_exact_sf
traffic_exact_sf <- traffic_exact_sf %>% 
  slice(1:100)

# density = 1/10 means every 10meters one point
# density = 10 means 10 points per metre
# density = 1 means 1 point per metre
sampled <- st_line_sample(traffic_exact_sf, density = 1)

st_sf(
  traffic_exact_sf %>% select(strassenklasse),
  geometry = sampled) %>% 
  mapview(zcol = "strassenklasse")
  #vect() %>% 
  #plot(y ="strassenklasse")

mapview(traffic_exact_sf[1:2,]) +
  mapview(sampled)

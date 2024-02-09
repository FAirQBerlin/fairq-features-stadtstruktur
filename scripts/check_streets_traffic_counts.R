devtools::load_all()

library(DT)
library(tidyverse)
library(readxl)
library(mapview)
library(terra)

library(RColorBrewer)
library(viridis)

library(mapview)
library(sf)
library(dplyr)

coords <- read.csv("data/relevant_coords.csv") %>% rename(element_nr = cmss.element_nr)

count_locations <- read.csv("data/count_coords.csv") %>%   
  st_as_sf(coords = c("lon", "lat"),
           crs = st_crs(4326)) %>%
  st_transform(25833)

coords %>% 
  distinct(element_nr)

relevant_element_nr <- 
c(
  '35550054_35550018.01', # Rominter ALlee
  '38510006_38500007.01', # Franzensbader Straße östl. Berkaer Straße (Charlottenburg-Wilmersdorf, 14193)"
  '43460046_44460015.01', # Ringstraße
  '44460015_44460002.01', # Ringstraße
  '38540009_38540010.02', # Knobelsdorfstr.
  '56440029_56440030.02', # Adlergestell
  '56440030_56440034.01' # Adlergestell
)

sel_coords <- coords %>% filter(element_nr %in% relevant_element_nr)

streets <- extract_raw_clickhouse("streets") %>% 
  select(strassenklasse1, element_nr, dtvw_kfz) %>%
  rename(strassenklasse = "strassenklasse1") %>%
  mutate(strassenklasse = factor(strassenklasse)) %>%
  st_transform(25833)


cell_centers <- sel_coords %>% st_as_sf(coords = c("x", "y"),
                      crs = st_crs(25833))

grids <- cell_centers %>% st_buffer(dist = 25, endCapStyle = "SQUARE")

relevant_streets <- grids %>%
  st_intersection(streets) %>% select(dtvw_kfz)

relvant_locations <- grids %>% st_intersection(count_locations)


mapview(grids) +
  mapview(relevant_streets) +
  mapview(relvant_locations)

## Anderers Problem:
# Ringstr.
# Adlergestell
# Franzensbader str (ungenau da wenig verkehr)

# Rominter Allee wird durch SPanndauer Damm / Charlottenburger Chausse nach oben gezogen
# Knobelsdorsgr. wird durch Stadtautobahn verfälscht (nach oben gezogen)

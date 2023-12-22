#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exclude streets of class V that are not accessible by cars
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(mapview)
devtools::load_all()

streets_raw <- extract_raw_clickhouse(
  "select * from stadtstruktur_network_streets;"
  ) %>% 
  mutate(strassenklasse = factor(strassenklasse),
         strassenklasse1 = factor(strassenklasse1),
         strassenklasse2 = factor(strassenklasse2))
glimpse(streets_raw)

streets_v <- streets_raw %>% 
  filter(strassenklasse1 == "V")

streets_v %>% 
  select(strassenklasse) %>% 
  summary()

streets_v %>% 
  pull(strassenklasse) %>% 
  levels()

# Which streets are we going to exclude fo category V?
# They all look non drivable
streets_v %>% 
  select(strassenklasse) %>% 
  filter(strassenklasse %in% c("F","P","N","X","W")) %>% 
  mapview()

# We keep only the ones with "G", they are drive-able
streets_v %>% 
  select(strassenklasse) %>% 
  filter(strassenklasse %in% c("G")) %>% 
  mapview()

traffic_raw %>% 
  filter(strassenklasse1 %in% c("0", "I", "II", "III", "IV") | 
         (strassenklasse1 == "V" & strassenklasse == "G")) %>% 
  select(strassenklasse) %>% 
  mapview()

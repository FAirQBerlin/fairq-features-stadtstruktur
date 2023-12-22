devtools::document()
devtools::load_all()

coord_mapping_stadt_streets <- data_sources()[["coord_mapping_stadt_streets"]] %>% 
  etl()

class(coord_mapping_stadt_streets$dat)
dim(coord_mapping_stadt_streets$dat)
summary(coord_mapping_stadt_streets$dat)

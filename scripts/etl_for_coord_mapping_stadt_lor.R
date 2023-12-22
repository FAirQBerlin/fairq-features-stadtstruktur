devtools::document()
devtools::load_all()

coord_mapping_stadt_lor <- data_sources()[["coord_mapping_stadt_lor"]] %>% 
  etl()

class(coord_mapping_stadt_lor$dat)
dim(coord_mapping_stadt_lor$dat)
summary(coord_mapping_stadt_lor$dat)

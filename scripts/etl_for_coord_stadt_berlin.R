devtools::document()
devtools::build()
devtools::load_all()

coord_stadt_berlin <- data_sources()[["coord_stadt_berlin"]] %>% 
  etl()

class(coord_stadt_berlin$dat)
ncol(coord_stadt_berlin$dat)
summary(coord_stadt_berlin$dat)

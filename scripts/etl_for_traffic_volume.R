devtools::document()
devtools::build()
devtools::load_all()

traffic_volume <- data_sources()[["traffic_volume"]] %>% 
  etl()

class(traffic_volume$dat)
ncol(traffic_volume$dat)
summary(traffic_volume$dat)

devtools::document()
devtools::build()
devtools::load_all()

traffic_intensity <- data_sources()[["traffic_intensity"]] %>% 
  etl()

class(traffic_intensity$dat)
ncol(traffic_intensity$dat)
summary(traffic_intensity$dat)

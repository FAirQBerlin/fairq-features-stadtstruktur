devtools::document()
devtools::build()
devtools::load_all()

land_use <- data_sources()[["land_use"]] %>% 
  etl()

class(land_use$dat)
ncol(land_use$dat)

df<- land_use$dat
rast <- df_to_rast(df)
plot(rast)

df %>% summary()


df_gewaesser <- df %>% 
  select(id:gewaesser)
rast_gewaesser <- df_to_rast(df_gewaesser)

vect_gewaesser <- extract_raw_clickhouse("land_use") %>% 
  prepare_land_use() %>% 
  filter(nutzung == "gewaesser") %>% 
  st_combine() %>% 
  vect()

plot(rast_gewaesser) 
plot(vect_gewaesser, add = T, alpha =1)
plot(vect_gewaesser)

mapview(as(rast_gewaesser, "Raster"),
        native.crs = TRUE)

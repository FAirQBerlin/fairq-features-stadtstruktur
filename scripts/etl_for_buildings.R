devtools::document()
devtools::build()
devtools::load_all()

# 1.) Run ETL

buildings <- data_sources()[["buildings"]] %>% 
  etl()

class(buildings$dat)
ncol(buildings$dat)
summary(buildings$dat)

# 2.) Check results

buildings_df <- send_query_clickhouse_out("select * from buildings;")
summary(buildings_df)
nrow(buildings_df)

buildings_only_rast <- buildings_df %>% 
  df_to_rast() 

# fairq_features.buildings has only 193784 rows - cells where there are buildings
# In all (?) other feature tables the full raster with 695844 cells is present.
# That is because "streets" , "traffic_intensity" etc. have 0s instead of NAs.
# 0s are written explicitly into the db with extra rows, NA rows are omitted.
buildings_rast <- extend(
  buildings_only_rast, 
  ext(create_berlin_rast())
  )
# plot(buildings_rast)

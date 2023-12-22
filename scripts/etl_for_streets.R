devtools::document()
devtools::build()
devtools::load_all()

streets <- data_sources()[["streets"]] %>% 
  etl()

class(streets$dat)
ncol(streets$dat)

streets_df <- 
  send_query_clickhouse_out("select * from streets;")


streets_rast <- df_to_rast(streets_df)

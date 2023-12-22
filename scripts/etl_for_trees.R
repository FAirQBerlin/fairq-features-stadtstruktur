devtools::document()
devtools::load_all()

# 1.) Run etl for trees

trees <- data_sources()[["trees"]] %>% 
  etl()

class(trees$dat)

# 2.) Check results for trees

trees_prepared <- extract_raw_clickhouse("trees") %>% 
  prepare_trees()
summary(trees_prepared)


trees_features <- send_query_clickhouse_out("select * from trees;")
# Have all trees been counted that went into the rasterization?
sum(trees_features$count) == nrow(trees_prepared)
summary(trees_features)

trees_rast <- df_to_rast(trees_features)
trees_vect <- trees_prepared %>% 
  st_transform(25833) %>% 
  vect()

# Check results visually:
# library("terra")
# plot(trees_rast)
# plot(trees_rast, "count")
# plot(trees_vect, add = T, cex = 0.005, alpha = 0.1)

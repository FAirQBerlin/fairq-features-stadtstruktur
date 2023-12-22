settings <- modules::module({
  creds <- function(db_name = Sys.getenv("DB_SCHEMA_IN")) {
    dbtools::Credentials(
      drv = RClickhouse::clickhouse,
      user = Sys.getenv("DB_USERNAME"),
      password = Sys.getenv("DB_PASSWORD"),
      db = db_name,
      host = Sys.getenv("DB_HOST"),
      port = as.integer(Sys.getenv("DB_PORT", 9000))
    )
  }
  db_retries <- 2
  db_int_sleep <- 10
  chunk_size <- 1e5
  

})

# For getting data from input db schema
creds_in <- settings$creds()
# For writing data into target db schema
creds_out <- settings$creds(db_name = Sys.getenv("DB_SCHEMA_OUT"))

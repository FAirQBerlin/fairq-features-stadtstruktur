# Send query ----

#' Send query wrapper for incoming database
#'
#' This is a wrapper around \link[dbtools]{sendQuery} to easily reference SQL files and
#' use credentials handled inside the package. This function is only for
#' "incoming" (=_in) database.
#'
#' @param query (character) a query as string or a file name.
#' @param ... arbitrary arguments for parameterized queries.
#'
#' @export
send_query_clickhouse_in <- function(query, ...) {
  l_send_query(creds_in,
               query,
               ...)
}

#' Send query wrapper for outgoing database
#'
#' This is a wrapper around \link[dbtools]{sendQuery} to easily reference SQL files and
#' use credentials handled inside the package. This function is only for
#' "outgoing" (=_out), i.e. target database.
#'
#' @param query (character) a query as string or a file name.
#' @param ... arbitrary arguments for parameterized queries.
#'
#' @export
send_query_clickhouse_out <- function(query, ...) {
    l_send_query(creds_out,
                 query,
                 ...)
  }

l_send_query <- function(cred, query, simplify = TRUE, ...) {
  log_debug("Sending query to %s@%s", cred$db, cred$host)
  dbtools::sendQuery(
    cred,
    get_query(query, ...),
    tries = settings$db_retries,
    intSleep = settings$db_int_sleep,
    simplify = simplify
  )
}

get_query <- function(fname, ...) {
  log_debug("Searching for query '%s' - maybe a file", fname)
  stopifnot(length(fname) == 1)
  if (grepl(";$", fname)) {
    return(dbtools::Query(fname, ...))
  }
  
  file_name <- system.file(sprintf("sql/%s.sql", fname),
                           package = environmentName(topenv()),
                           mustWork = TRUE)
  
  dbtools::Query(file(file_name), ..., keepComments = FALSE)
}

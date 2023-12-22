#' Single source
#'
#' @param name (character) a unique name used as class and to identify target
#'   table on database.
#' @param get_data (function)
#' @param load_mode (character) defaults to "replace"
#' @param send_data (function)
#' @param ... arguments used within methods and available for queries
#'
#' @export
#' @rdname single_source
single_source <- function(name,
                          get_data = extract_raw_clickhouse,
                          load_mode = "replace",
                          send_data = send_data_clickhouse,
                          ...) {
  # name (character) defines the class and is used for logging
  # get_data (function) a function that given the name and ... can extract data
  # ... passed to send data or otherwise used by methods
  out <- list(
    name = name,
    get_data = get_data,
    load_mode = load_mode,
    send_data = send_data,
    ...
  )
  class(out) <- c(name, "list")
  
  return(out)
}

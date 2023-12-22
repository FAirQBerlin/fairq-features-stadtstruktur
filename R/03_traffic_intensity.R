

#' Transform for traffic intensity
#'
#' @param x clickhouse single_source
#' @param ... other parameters
#'
transform.traffic_intensity <- function(x, ...) {
  id <- y <- NULL # fix linting
  
  street_lengths <- x$dat
  
  traffic_intensity <- street_lengths %>%
    select(id, x, y) %>%
    mutate(
      traffic_intensity_kfz = calc_traffic_intensity("kfz", street_lengths),
      traffic_intensity_lkw = calc_traffic_intensity("lkw", street_lengths)
    )
  
  x$dat <- traffic_intensity
  
  x
}


#' Get Berlin wide overall average of traffic intensity
#'
#' Get Berlin wide overall average of traffic intensity as unscaled scalar.
#' Traffic intensity unscaled is defined as product of street length in metres
#' multiplied with number of vehicles - all by street type.
#' Overall average is then the sum over all street types.
#'
#' @param vehicle_type character, either "kfz" or "lkw"
#' @param street_lengths data.frame, e.g. output from "traffic_intensity" query
#' @return numeric scalar, overall average of traffic intensity unscaled
#'
#' @export
get_overall_avg_traffic_intensity_unscaled <- function(vehicle_type,
                                                       street_lengths) {
  length_vehicles <- vehicles <- NULL # fix linting
  
  # 1.) Get street length and traffic avgs by street type
  street_length_avgs_long <- get_street_length_avgs(street_lengths)
  traffic_avgs_long <- get_traffic_avgs(vehicle_type) %>%
    pivot_traffic_avgs_longer()
  
  # 2.) Join street length and traffic avgs
  street_length_traffic_avgs_long <-
    left_join(street_length_avgs_long,
              traffic_avgs_long,
              by = "strassenklasse")
  
  # 3.) Product of street length & number of vehicles, sum over all street types
  overall_avg_traffic_intensity_unscaled <-
    street_length_traffic_avgs_long %>%
    mutate(length_vehicles = length * vehicles) %>%
    summarize(overall_avg = sum(length_vehicles)) %>%
    pull()
  
  return(overall_avg_traffic_intensity_unscaled)
}



#' Get average street lengths per cell by strassenklasse
#'
#' @param street_lengths data.frame, e.g. output from "traffic_intensity" query
#' @return data.frame in long format with columns "strassenklasse" and "length"
#'
#' @export
get_street_length_avgs <- function(street_lengths) {
  
  # nolint start
  strassenklasse <-
    strassenklasse_0 <- strassenklasse_V <- NULL # fix linting
  # nolint end
  
  street_lengths %>%
    # Caution: Only for cells where there are streets at all!
    filter(if_any(starts_with("strassenklasse"), ~ . > 0)) %>%
    summarize_at(vars(strassenklasse_0:strassenklasse_V), mean) %>%
    pivot_longer(
      cols = starts_with("strassenklasse"),
      names_to = "strassenklasse",
      values_to = "length"
    ) %>%
    mutate(strassenklasse = gsub("strassenklasse_", "", strassenklasse))
}


#' Get average street lengths per raster cell by street type
#'
#' @param traffic_avgs_wide data.frame, e.g. get_traffic_avgs()
#' @return data.frame in long format with columns "strassenklasse" and "length"
#'
#' @export
pivot_traffic_avgs_longer <- function(traffic_avgs_wide) {
  strassenklasse <- NULL # fix linting
  
  vehicle_type <- sub("_.*", "", names(traffic_avgs_wide)) %>%
    unique()
  
  traffic_avgs_long <- traffic_avgs_wide %>%
    pivot_longer(cols = everything(),
                 names_to = "strassenklasse",
                 values_to = "vehicles") %>%
    mutate(strassenklasse = gsub(paste0(vehicle_type, "_"),
                                 "",
                                 strassenklasse))
  
  return(traffic_avgs_long)
}


#' Calculates scaled traffic intensity per raster cell.
#'
#' Scaling factor is the Berlin overall average of unscaled traffic intensity.
#' Interpretation of traffic intensity is:
#'    1 = traffic equals that of an average cell which has streets in it
#'    smaller than 1 = less traffic than an average cell which has streets in it
#'    bigger than 1 = more traffic than an average cell which has streets in it
#'
#' @param vehicle_type character, either "kfz" or "lkw"
#' @param street_lengths data.frame, e.g. output from "traffic_intensity" query
#'                       where each row represents one raster cell
#'
#' @return numeric vector, traffic intensity for vehicle type scaled
#'
#' @export
calc_traffic_intensity <- function(vehicle_type,
                                   street_lengths) {
  # 1.) Get data
  street_lengths_traffic_avgs <- street_lengths %>%
    cbind(get_traffic_avgs(vehicle_type)) %>%
    as.data.frame() # drop data.table class
  
  # 2.) Prepare col names for unscaled traffic intensity calculation
  strassenklassen <- c("0", "I", "II", "III", "IV", "V")
  strassenklassen_cols <- paste0("strassenklasse_", strassenklassen)
  vehicle_cols <- paste0(vehicle_type, "_", strassenklassen)
  
  # 3.) Create unscaled traffic intensity
  # nolint start
  traffic_intensity_unscaled <-
    rowSums(street_lengths_traffic_avgs[, strassenklassen_cols] *
              street_lengths_traffic_avgs[, vehicle_cols])
  # nolint end
  
  # 4.) Scale traffic intensity
  traffic_intensity <- traffic_intensity_unscaled /
    get_overall_avg_traffic_intensity_unscaled(vehicle_type, street_lengths)
  
  return(traffic_intensity)
}

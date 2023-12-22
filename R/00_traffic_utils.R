
#' Prepare raw streets data for various features
#'
#' @param streets_raw_sf sf object of raw streets (spatial lines) data
#' @param as_vect bool, shall return value be a terra SpatVect object?
#'                Defaults to TRUE. If false, an sf object is returned.
#' @return SpatVect terra object in crs ESPG:25833, sf object if
#'         as_vect = FALSE
#'
#' @export
prepare_streets <- function(streets_raw_sf, as_vect = TRUE) {
  strassenklasse <- NULL # fix package build binding problem
  
  streets <- streets_raw_sf %>%
    exclude_non_drivable_streets() %>% 
    rename(strassenklasse = "strassenklasse1") %>%
    mutate(strassenklasse = factor(strassenklasse)) %>%
    st_transform(25833)
  
  if (as_vect) {
    streets <- vect(streets)
  }
  
  return(streets)
  
}


#' Exclude all streets of category V which are non car drivable.
#'
#' More info on categories here:
#' https://de.wikipedia.org/wiki/%C3%9Cbergeordnetes_Stra%C3%9Fennetz_von_Berlin
#' "G" stands for Gemeindestraße, categories such as "F" (Fußgängerobjekte) get
#' excluded. This functtion reduces the number of streets from roughly 43000 to
#' 37000.
#'
#' @param streets_raw_sf sf object of raw streets (spatial lines) data
#' @return sf object of streets. All streets in this sf object all car drivable.
#'         Walking paths etc. were excluded.
#'
#' @export
exclude_non_drivable_streets <- function(streets_raw_sf) {
   
  strassenklasse <- strassenklasse1 <- NULL # fix linting
  
   streets_only_drivable <- streets_raw_sf %>%
    filter(
      strassenklasse1 %in% c("0", "I", "II", "III", "IV") |
        (strassenklasse1 == "V" & strassenklasse == "G")
    ) %>%
    select(-strassenklasse)
  
  return(streets_only_drivable)
}


#' Get average traffic numbers per street type for a particular vehicle type
#'
#' @param vehicle_type character, either "kfz" or "lkw"
#' @return data.frame in wide format with only one row for vehicle_type
#'
#' @export
get_traffic_avgs <- function(vehicle_type) {
  kfz <- lkw <- strassenklasse <- NULL # fix linting
  
  strassenklassen <- c("0", "I", "II", "III", "IV", "V")
  
  traffic_avgs <- send_query_clickhouse_in("traffic_averages") %>%
    mutate(strassenklasse = factor(strassenklasse,
                                   levels = strassenklassen)) %>%
    arrange(strassenklasse) %>%
    pivot_wider(names_from = strassenklasse,
                values_from = kfz:lkw) %>%
    select(starts_with(vehicle_type))
  
  return(traffic_avgs)
}

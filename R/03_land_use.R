# land_use ----

#' Transform for land_use
#'
#' @param x clickhouse single_source
#' @param cell_size cell_size of regular target rast, measured in metres
#' @param ... other parameters
transform.land_use <- function(x, cell_size = 50, ...) {
  
  ### Prepare SpatRaster object ###
  berlin_rast <- create_berlin_rast(cell_size)
  ### Prepare land_use ###
  land_use <- prepare_land_use(x$dat)
  land_use_categories <- levels(land_use$nutzung)
  
  ### Calculate land_use coverage for each use category ###
  for (i in land_use_categories) {
    logging("~~~~~~ Starting land use category %s. ~~~~~~", i)
    
    nutzung <- NULL # fix linting
    
    land_use_filtered <- land_use %>% 
      filter(nutzung == i) %>% 
      st_combine() # merge all polygons into one
    
    land_use_rast <- coverage_fraction(berlin_rast, land_use_filtered)[[1]]
    names(land_use_rast) <- paste(i)

    rm(land_use_filtered); gc()
    add(berlin_rast) <- land_use_rast
    rm(land_use_rast); gc()
    
    logging("~~~~~~ Finishing land use category %s. ~~~~~~", i)
    
  }
  
  ### Transform SpatRaster into df ###
  land_use_df <- rast_to_df(berlin_rast)
  
  x$dat <- land_use_df
  x
}


#' Prepare raw land use data for coverage calculation and collapse use categories.
#'
#' @param land_use_raw sf object of raw land_use (polygons) data
#' @return sf object of clean land_use data with collapsed factor levels for use
#'
#' @description transform the land_use data
prepare_land_use <- function(land_use_raw) {
  
  nutzung <- NULL # fix linting
  
  land_use <- land_use_raw %>%
    st_transform(25833) %>%
    rename(nutzung = "nutzung_bauvor") %>%
    mutate(nutzung = factor(nutzung))
  
  # Categories are explained here:
  # https://www.berlin.de/umweltatlas/nutzung/flaechennutzung/2020/kartenbeschreibung/
  land_use <- land_use %>%
    mutate(
      nutzung = fct_collapse(
        nutzung,
        wohnnutzung = "Wohnnutzung",
        wald = "Wald",
        gewaesser = "Gew\u00e4sser",
        mischnutzung = "Mischnutzung",
        gruenflaeche = c(
          "Park / Gr\u00fcnfl\u00e4che",
          "Kleingartenanlage",
          "Wochenendhaus- und kleingarten\u00e4hnliche Nutzung",
          "Friedhof",
          "Ackerland",
          "Brachfl\u00e4che, Mischbestand aus Wiesen, Geb\u00fcschen und B\u00e4umen",
          "Brachfl\u00e4che, wiesenartiger Vegetationsbestand",
          "Gr\u00fcnland",
          "Baumschule / Gartenbau"
        ),
        grauflaeche = c(
          "Verkehrsfl\u00e4che (ohne Stra\u00dfen)",
          "Brachfl\u00e4che, vegetationsfrei",
          "Stadtplatz / Promenade"
        ),
        infrastruktur = c(
          "Gewerbe- und Industrienutzung, gro\u00dffl\u00e4chiger Einzelhandel",
          "Gemeinbedarfs- und Sondernutzung",
          "Kerngebietsnutzung",
          "Ver- und Entsorgung",
          "Baustelle"
        )
      )
    ) %>%
    # We want alphabetical level order:
    mutate(nutzung = factor(nutzung, levels = sort(levels(nutzung))))
  
  return(land_use)
  
}

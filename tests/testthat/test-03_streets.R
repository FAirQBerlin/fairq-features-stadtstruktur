
test_that(
  "Check if calc_lines_length_per_cell gives plausible return value",
  {
    ### Arrange
    berlin_rast <- create_berlin_rast(3000)
    lines_vect <- get_berlin_bezirke() %>% 
      st_cast("MULTILINESTRING") %>% 
      st_transform("EPSG:25833") %>% 
      vect()
    expected_proj <- st_crs(25833)$proj4string
    # nolint start
    # library(terra)
    # plot(berlin_rast)
    # plot(lines_vect, add = T)
    # nolint end
    
    ### Act
    res <- calc_line_length_per_cell(
      berlin_rast, lines_vect
    )
    res_proj <- st_crs(crs(res))$proj4string
    
    ### Assert
    # nolint start
    # plot(res)
    # plot(lines_vect, add = T)
    # text(res, cex = 0.5)
    # nolint end
    
    expect_true("SpatRaster" %in% class(res))
    expect_true(names(res) == "length")
    expect_true(res_proj == expected_proj)
  }
)

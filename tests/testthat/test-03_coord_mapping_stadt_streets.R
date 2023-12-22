
test_that("Check if calc_traffic_intensity gives plausible return values",
          {
            ### Arrange
            line_1 <- rbind(c(4.23, 1.29), c(1, 5), c(2, 5)) %>%
              st_linestring()
            line_2 <- rbind(c(0, 1.11), c(2.99, 4.88)) %>%
              st_linestring()
            geometry <- list(line_1, line_2)
            lines_sf <- st_sf(value = c(10, 1),
                              geometry,
                              crs = st_crs(25833))
            
            geometry <- list(round(line_1), round(line_2)) # rounded geometry
            expected_sf <- st_sf(value = c(10, 1),
                                 geometry,
                                 crs = st_crs(25833))
            
            ### Act
            res <- round_line_coordinates(lines_sf, decimals = 0)
            
            ### Assert
            # expect_equal does not work because of different bboxes
            expect_true(all(expected_sf == res)) # calculation OK?
            
            ### Assert
            # nolint start
            # Check result visually
            # library(mapview)
            # mapview(res) + 
            #  mapview(expected_sf, color = "red")
            # nolint end
            
          })

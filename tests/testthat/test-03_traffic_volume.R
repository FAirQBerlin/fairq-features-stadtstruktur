
test_that("Check if rasterization of lines works correctly",
          {
            ### Arrange
            line_1 <- rbind(c(4, 1), c(1, 5), c(2, 5)) %>%
              st_linestring()
            line_2 <- rbind(c(0, 1), c(2, 4)) %>%
              st_linestring()
            lines <- list(line_1, line_2)
            
            # Inputs for function
            test_lines <- st_sf(value = c(10, 1),
                                lines,
                                crs = st_crs(25833))
            test_rast <- rast(
              xmin = 0,
              xmax = 6,
              ymin = 0,
              ymax = 6,
              resolution = 2,
              crs = st_crs(25833)$wkt
            )
            # Points not needed for as input, just for visual checking
            n <- 10
            test_points <- lines_to_points(test_lines, n)
            
            expected_df <- data.frame(
              id = 1:9,
              x = rep(c(1, 3, 5), times = 3),
              y = rep(c(5, 3, 1), each = 3),
              value = c(10, 0, 0, 10, 10, 0, 1, 10, 0)
            )
            
            ### Act
            res <- rasterize_lines_max(test_lines,
                                       test_rast,
                                       "value",
                                       n = n)
            res_df <- res %>%
              rast_to_df()
            
            ### Assert
            # nolint start
            # Check result visually
              # library(terra)
              # plot(res, legend = NA)
              # text(res, add = T)
              # plot(vect(test_lines), "value", add = T, legend = "topleft")
              # plot(vect(test_points), "value", add = T, legend = "bottomleft")
            # nolint end
            expect_equal(res_df, expected_df)
            expect_true("SpatRaster" %in% class(res))
            
          })


test_that("Check if get_traffic_volume_avg gives plausible return values",
          {
            ### Arrange
            expected_cols <-
              c("strassenklasse", "avg_traffic_volume", "geometry")
            ### Act
            res <- get_traffic_volume_avg("kfz")
            res_unique_volumes <-
              length(unique(res$avg_traffic_volume))
            ### Assert
            expect_true("sf" %in% class(res))
            expect_true("sfc_LINESTRING" %in% class(res$geometry))
            expect_true(st_crs(res) == st_crs(25833))
            expect_equal(6, res_unique_volumes)
            expect_equal(expected_cols, names(res))
          })

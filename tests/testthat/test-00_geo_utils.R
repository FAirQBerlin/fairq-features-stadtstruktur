
test_that("Check if calculating area weighted values via polygon rasterization
          correctly",
          {
            ### Arrange
            
            # Inputs for function
            poly_obj <-
              system.file("shape/nc.shp", package = "sf") %>%
              st_read() %>%
              st_transform(25833) %>%
              slice(1:3) %>%
              select(BIR74) %>%
              rename(values = "BIR74") %>%
              mutate(values = c(1, 5, 10)) %>%
              vect()
            
            rast_obj <- rast(
              # Make extent bigger by buffer
              buffer(poly_obj, width = 50000),
              ncol = 3,
              nrow = 3,
              crs = crs(poly_obj)
            )
            rast_obj <- init(rast_obj, 1:ncell(rast_obj))
            # Make rast_obj look like a create_berlin_rast() with id column
            names(rast_obj) <- "id"
            
            # Create expected df
            expected_df <- data.frame(
              # cells with raster id 1 and 9 are empty (with NAs), hence
              # not present in the resulting df
              id = 2:8,
              x = c(
                -6506757.6,
                -6441940.6,
                -6571574.7,
                -6506757.6,
                -6441940.6,
                -6571574.7,
                -6506757.6
              ),
              y = c(
                11002938.9,
                11002938.9,
                10904234.3,
                10904234.3,
                10904234.3,
                10805529.7,
                10805529.7
              ),
              values = c(1, 1, 10, 6, 1.9, 10, 10)
            )
            rownames(expected_df) <- as.integer(expected_df$id)
            
            ### Act
            res <- calc_area_weighted_values(rast_obj, poly_obj, "values")
            
            res_df <- res %>%
              rast_to_df() %>%
              round(1) # makes it easier to test
            
            ### Assert
            # nolint start
            # Check result visually
            # plot(res["values"])
            # plot(poly_obj,
            #      y = 1,
            #      add = T,
            #      legend = "topleft")
            # text(res["values"], digits = 2)
            # nolint end
            
            expect_true("SpatRaster" %in% class(res))
            expect_equal(res_df, expected_df)
          })

test_that("Check if lines_to_points gives plausible return values",
          {
            ### Arrange
            line_1 <- rbind(c(4, 1), c(1, 5), c(2, 5)) %>%
              st_linestring()
            line_2 <- rbind(c(0, 1), c(2, 4)) %>%
              st_linestring()
            lines <- list(line_1, line_2)
            lines_sf <- st_sf(value = c(10, 1),
                              lines,
                              crs = st_crs(25833))
            ### Act
            res <- lines_to_points(lines_sf, 2)
            ### Assert
            expect_true("sf" %in% class(res))
            expect_true("POINT" %in% unique(st_geometry_type(res)))
            expect_true(nrow(res) == 19)
            expect_true(all(names(res) == c("value", "geometry")))
          })

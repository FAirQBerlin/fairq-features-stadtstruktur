
test_that("Test tree feature calculation",
          {
            ### Arrange
            # 3 trees that fall into 2 of the 4 cells of the raster
            trees_df <- data.frame(
              x = c(0.3, 1.2, 1.4),
              y = c(0.3, 1.1, 1.5),
              standalter = c(100, 10, 50),
              baumhoehe = c(10, 1, 5)
            )
            trees_vect <- st_as_sf(trees_df,
                                   coords = c("x", "y"),
                                   crs = 25833) %>%
              vect()
            
            r <- rast(
              nrows = 2,
              ncols = 2,
              xmin = 0,
              xmax = 2,
              ymin = 0,
              ymax = 2,
              crs = st_crs(25833)$wkt
            )
            r <- init(r, 1:ncell(r))
            names(r) <- "id"
            
            expected_df <- data.frame(
              id = 2:3,
              x = c(1.5, 0.5),
              y = c(1.5, 0.5),
              count = c(2, 1),
              age = c(30, 100),
              height = c(3, 10)
            )
            rownames(expected_df) <- as.integer(expected_df$id)
            
            ### Act
            res <- calc_tree_features(trees_vect, r)
            res_df <- rast_to_df(res)
            
            ### Assert
            
            # nolint start
            # Visual check:
            # library(terra)
            # par(mfrow = c(1, 3))
            # plot(res["count"], main = "count")
            # plot(trees_vect, add = T)
            # plot(res["age"], legend = "bottomright", main = "age")
            # plot(trees_vect, "standalter", add = T)
            # plot(res["height"], legend = "bottomright", main = "height")
            # plot(trees_vect, "baumhoehe", add = T)
            # nolint end
            
            expect_true("SpatRaster" %in% class(res))
            expect_equal(res_df, expected_df)
          })

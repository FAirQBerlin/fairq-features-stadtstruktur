

test_that("create_berlin_stars() returns a stars object
          with correct cell size and crs",
          {
            # Act
            res_default <- create_berlin_stars()
            res_2000 <- create_berlin_stars(cell_size = 2000)
            res_wgs84 <- create_berlin_stars(target_crs = 4326)
            
            # Assert
            expect_true("stars" %in% class(res_default))
            expect_true(st_crs(res_default) == st_crs(25833))
            expect_true("stars" %in% class(res_2000))
            # with 2000m x 2000m extension we get 25 x 21 cells
            expect_true(all(unname(dim(res_2000)) == c(25, 21)))
            expect_true("stars" %in% class(res_wgs84))
            expect_true(st_crs(res_wgs84) == st_crs(4326))
          })


test_that("create_berlin_rast() returns a SpatRaster of expected format",
          {
            # Arrange
            expected_proj <- st_crs(25833)$wkt
            expected_ncells <- 695844
            expected_ncells_1000 <- 1920
            
            # Act
            res <- create_berlin_rast()
            res_1000 <- create_berlin_rast(1000)
            
            res_proj <- crs(res)
            res_ncells <- ncell(res)
            res_ncells_1000 <- ncell(res_1000)
            
            # Assert
            expect_true("SpatRaster" %in% class(res))
            expect_true("id" == names(res))
            expect_true(res_proj == expected_proj)
            
            expect_true(res_ncells == expected_ncells)
            expect_true(res_ncells_1000 == expected_ncells_1000)
          })

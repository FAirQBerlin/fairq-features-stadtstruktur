

test_that("test if individual data sources are named correctly",
          {
            # Arrange
            data_sources <- list(single_source(name = "bla"),
                                 single_source(name = "blupp"))
            
            # Act
            res <- name_data_sources(data_sources)
            
            # Assert
            expect_true(all(names(res) == c("bla", "blupp")))
            
          })

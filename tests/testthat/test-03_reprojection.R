# FUNCTION reproject() -----------------------------------------------------------------------------
test_that("reproject works", {
  # arrange
  load(file.path("testdata", "reprojection.RData"))
  coordinates <- data.frame(
      lat = c(52.596169, -41.526575, 48.004911),
      lon = c(12.132468, -69.539240, 2.051376)
    )

  # act
  res <- reproject(coordinates)

  # test
  expect_equal(res$geometry[[1]], reprojection$geometry[[1]])

})

# FUNCTION prepare_mapping() -----------------------------------------------------------------------
test_that("prepare_mapping works", {
  # arrange
  load(file.path("testdata", "reprojection_mapping_table.RData"))
  load(file.path("testdata", "reprojection.RData"))
  
  # act
  res <- prepare_mapping(reprojection)
  
  # test
  expect_equal(res, reprojection_mapping_table)
})

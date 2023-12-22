


test_that(
  "extract_raw_clickhouse returns an sf object -
          if queried data contains a 'geometry' geojson column",
  {
    # Arrange
    query_example <-
      "select * from stadtstruktur_network_edifice limit 10;"
    query_error_example <-
      "select * from ref_coordinate_systems;"
    # Act
    res <- extract_raw_clickhouse(query_example)
    # Assert
    expect_true("sf" %in% class(res))
    expect_true("sfc_MULTILINESTRING" %in% class(res$geometry))
    expect_true(nrow(res) == 10)
    expect_error(extract_raw_clickhouse(query_error_example))
  }
)

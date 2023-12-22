test_that("Package code is in line with our style conventions", {
  # Allow object names of length 35 instead of the default 30
  linters <- INWTUtils::selectLinters(
    excludeLinters = "object_length_linter",
    addLinters = c(
      object_length_linter = lintr::object_length_linter(length = 35L)
      )
  )
  lintr::expect_lint_free(linters = linters)
})

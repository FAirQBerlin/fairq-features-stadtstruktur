library(testthat)
library(lintr)
library(fairqFeaturesStadtstruktur)

Sys.setenv(NOT_CRAN = "true")

test_check("fairqFeaturesStadtstruktur")

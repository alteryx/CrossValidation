#### Example tests using testthat library.

ayxPath <- "C:/Program Files/Alteryx/R-3.2.3/library"

.libPaths(c(.libPaths(), ayxPath))


library(testthat)

library(AlteryxPredictive)


#source("C:/SVN Repositories/Content Sandboxes/BToomey Sandbox/Cross-validation/cv_v3_r_code.R")

vector1 <- c("col1", "col3", "col2")
vector2 <- c("col12", "col3", "col5")
vector3 <- c("col2", "col1", "col3")

test_that("areIdentical returns true when different ordered but same vectors are passed", {
  expect_that(areIdentical(vector1, vector3), equals(TRUE))
})

test_that("areIdentical returns false when different vectors are passed", {
  expect_that(areIdentical(vector1, vector3), equals(FALSE))
})


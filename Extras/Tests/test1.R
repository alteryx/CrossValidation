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

###########################################################
############################################################

# Read in test data and model for purpose of testing checkXVars function
test_data <- read.csv("C:/CrossValidation/Extras/Tests/jp_test.csv")
# Assumption is that Alteryx will set the target variable to the first column
# of the data set.
targetVar <- c("smoker")
test_data <- test_data[, c(targetVar, setdiff(names(test_data), targetVar))]

#Specify the object list which contains the test data and the model object
test_defaults <- list(
  d1 = test_data,
  d2 = rbind(
    readRDS("C:/RDS/random_forest_1.rds") 
  )
)
# model name should be the same as in RDS file
modelNames <- c("random_forest")
# Create input list for passing to checkXVars
inputs <- list(
  data = read.Alteryx2("#1", default = test_defaults$d1),
  models = readModelObjects("#2", default = test_defaults$d2),
  modelNames = modelNames)


test_that("CheckXVars doesn't return errors when passed good data and model object", {
  expect_that(checkXVars(inputs),equals(NULL))
})
####
## Set up for second test of checkXVars

## Create "Bad" data
test_defaults2 <- list(
  d1 = read.csv("C:/CrossValidation/Extras/Tests/jp_test.csv"),
  d2 = rbind(
    readRDS("C:/RDS/random_forest_1.rds") 
  )
)

# model name should be the same as in RDS file
modelNames <- c("random_forest")
# Create input list for passing to checkXVars
bad_inputs <- list(
  data = read.Alteryx2("#1", default = test_defaults2$d1),
  models = readModelObjects("#2", default = test_defaults2$d2),
  modelNames = modelNames)

test_that("CheckXVars returns errors when passed non-matching model and data", {
  expect_that(checkXVars(bad_inputs), throws_error())
})

empty_inputs <- list()

test_that("CheckXVars returns errors when passed null data", {
 expect_that(checkXVars(empty_inputs), throws_error()) 
})



checkXVars(empty_inputs)

names(inputs$data)

modelXVars[[1]] %in% dataXVars
modelXVars[[1]]
dataXVars

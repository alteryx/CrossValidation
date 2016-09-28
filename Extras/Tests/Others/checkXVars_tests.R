#ayxPath <- "C:/Program Files/Alteryx/R-3.2.3/library"

.libPaths(c(.libPaths(), ayxPath))


library(testthat)

library(AlteryxPredictive)


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

####
## Set up for second test of checkXVars

## Create "Bad" data
test_defaults2 <- list(
  d1 = read.csv("C:/CrossValidation/Extras/Tests/jp_test.csv"),
  d2 = rbind(
    readRDS("C:/RDS/random_forest_1.rds") 
  )
)


# Create input list for passing to checkXVars
bad_inputs <- list(
  data = read.Alteryx2("#1", default = test_defaults2$d1),
  models = readModelObjects("#2", default = test_defaults2$d2),
  modelNames = modelNames)


#### Test 3 Data

empty_inputs <- list()

## Tests #######################
test_that("CheckXVars doesn't return errors when passed good data and model object", {
  expect_that(checkXVars(inputs),equals(NULL))
})

test_that("CheckXVars returns errors when passed non-matching model and data", {
  expect_that(checkXVars(bad_inputs), throws_error())
})

test_that("CheckXVars returns errors when passed null data", {
 expect_that(checkXVars(empty_inputs), throws_error()) 
})
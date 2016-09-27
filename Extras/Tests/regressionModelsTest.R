#Regression Tests

ayxPath <- "C:/Program Files/Alteryx/R-3.2.3/library"

.libPaths(c(.libPaths(), ayxPath))


library(testthat)

library(AlteryxPredictive)

source("C:/CrossValidation/Supporting_Macros/CrossValidation1.R")

config <- list(
 `classification` = radioInput('%Question.classification%' , FALSE),
 `displayGraphs` = checkboxInput('%Question.displayGraphs%' , FALSE),
 `numberFolds` = numericInput('%Question.numberFolds%' , 5),
 `numberTrials` = numericInput('%Question.numberTrials%' , 3),
 `posClass` = textInput('%Question.posClass%'),
 `predFields` = listInput('%Question.predFields%'),
 `regression` = radioInput('%Question.regression%' , TRUE),
 `stratified` = checkboxInput('%Question.stratified%' , FALSE),
 `targetField` = dropdownInput('%Question.targetField%')
)
options(alteryx.wd = '%Engine.WorkflowDirectory%')
options(alteryx.debug = config$debug)
##----

#' ### Defaults
#' 
#' These defaults are used when the R code is run outside Alteryx
macroDirectory <- textInput('%Engine.WorkflowDirectory%', "Supporting_Macros")
dataDir <- file.path(macroDirectory, "Data")


test_data <- read.csv("C:/CrossValidation/Extras/Tests/jp_test.csv")
# Assumption is that Alteryx will set the target variable to the first column
# of the data set.
targetVar <- c("charges")
test_data <- test_data[, c(targetVar, setdiff(names(test_data), targetVar))]

#Specify the object list which contains the test data and the model object
test_mods <- readRDS("C:/CrossValidation/Supporting_Macros/Data/regressionModels.rds")
test_mods <- readModelObjects('#2', default=test_mods)

#test helper function  

runCvTest <- function(model, data, config) {
  cvInputs <- list(
    data = data,
    models = model
  )
 cvInputs$modelNames = names(cvInputs$models) 
 cvResults(config = config, data = cvInputs$data, models = cvInputs$models, modelNames = cvInputs$modelNames)
}

#boosted regression
test_that("Boosted regression model passes", {
  expect_that(runCvTest(test_mods[1], test_data, config),throws_error(NA) )
})

#decistion tree regression
test_that("Decision tree regression model passes", {
  expect_that(runCvTest(test_mods[2], test_data, config),throws_error(NA) )
})
#gamma regression
test_that("Gamma regression model passes", {
  expect_that(runCvTest(test_mods[3], test_data, config),throws_error(NA) )
})
#linear regression
test_that("Linear regression model passes", {
  expect_that(runCvTest(test_mods[4], test_data, config),throws_error(NA) )
})
#spline
test_that("Spline model passes", {
  expect_that(runCvTest(test_mods[5], test_data, config),throws_error(NA) )
})

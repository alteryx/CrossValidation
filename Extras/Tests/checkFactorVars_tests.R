#ayxPath <- "C:/Program Files/Alteryx/R-3.2.3/library"

.libPaths(c(.libPaths(), ayxPath))


library(testthat)

library(AlteryxPredictive)


source("C:/CrossValidation/Supporting_Macros/CrossValidation1.R")

data <- inputs$data 
models <- inputs$models 
modelNames <- inputs$modelNames

target <- data[, 1]
foldList <- generateCVRuns(labels = target, 
                           ntimes = config$numberTrials, 
                           nfold = config$numberFolds, 
                           stratified = config$stratified)

test_that("checkFactorVars returns warnings and messages when provided test data", {
  expect_that(checkFactorVars(data = data, folds = foldList, config = config), shows_message() )
})

# Need to write tests that pass in bad data to check error handling.  

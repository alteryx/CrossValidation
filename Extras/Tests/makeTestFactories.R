library(testthat)
library(plyr)
source('Supporting_Macros/CrossValidation1.r')

config <- list(
  `classification` = TRUE,
  `displayGraphs` = FALSE,
  `numberFolds` = 5,
  `numberTrials` = 1,
  `posClass` = "",
  `regression` = FALSE,
  `stratified` = FALSE,
  `targetField` = 'smoker'
)

runTest <- function(inputs, config, msg){
  message('Testing model ', msg)
  test_that(paste(modelName, 'passes'), {
    expect_that(runCrossValidation(inputs, config), throws_error(NA) )
  })
}

makeInputs <- function(csvFile, modelsFile, targetVar){
  test_data <- read.csv(csvFile)
  list(
    data = test_data[, c(targetVar, setdiff(names(test_data), targetVar))],
    models = AlteryxPredictive::readModelObjects("2", readRDS(modelsFile))
  )
}

runTest <- function(modelName, payload){
  inputs <- list(data = payload$data, models = payload$models[modelName])
  message(paste(modelName, 'passes'))
  test_that(paste(modelName, 'passes'), {
    expect_that(invisible(runCrossValidation(inputs, config)), throws_error(NA) )
  })
}

payload <- makeInputs(
  'Extras/Tests/jp_test.csv', 
  'Supporting_Macros/Data/classificationModels.rds',
  config$targetField
)

l_ply(names(payload$models[-1]), failwith(f = runTest), payload = payload)






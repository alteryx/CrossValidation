library(testthat)
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

#' Read Data and Models
test_data <- read.csv("Extras/Tests/jp_test.csv")
targetVar <- c("smoker")
test_data <- test_data[,c(targetVar, setdiff(names(test_data), targetVar))]

test_mods <- readRDS("Supporting_Macros/Data/classificationModels.rds")
test_mods <- AlteryxPredictive::readModelObjects("#2", test_mods)


inputs <- list(
  data = test_data,
  models = test_mods[1]
)


runTest <- function(id){
  inputs$models = test_mods[id]
  modelName = names(inputs$models)
  message('Testing model ', modelName)
  test_that(paste(modelName, 'passes'), {
    expect_that(runCrossValidation(inputs, config), throws_error(NA) )
  })
}

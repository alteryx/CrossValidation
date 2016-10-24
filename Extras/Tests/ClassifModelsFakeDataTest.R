#' ### Classification Model Tests
library(testthat)
library(plyr)
source('Supporting_Macros/CrossValidation1.r')

config <- list(
  classification = TRUE,
  displayGraphs = FALSE,
  numberFolds = 5,
  numberTrials = 3,
  posClass = "",
  regression = FALSE,
  stratified = FALSE
)

payload <- makePayload(
  'Extras/Tests/Data/FakeData.csv',
  'Extras/Tests/Data/RandomForest_FakeData.rds'
)

set.seed(1234)
inputs <- list(data = payload$data, models = payload$models[1])
d <- runCrossValidation(inputs, config)

#l_ply(names(payload$models), failwith(f = runTest), payload = payload)


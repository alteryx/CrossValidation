#' ### Regression Model Tests
library(testthat)
library(plyr)
source('Supporting_Macros/CrossValidation1.r')

config <- config <- list(
  classification = FALSE,
  regression = TRUE,
  displayGraphs = FALSE,
  numberFolds = 5,
  numberTrials = 1,
  posClass = "",
  regression = FALSE,
  stratified = FALSE
)



payload <- makePayload(
  'Extras/Tests/Data/Insurance.csv',
  'Extras/Tests/Data/AllRegModelsInsurance.rds'
)


l_ply(names(payload$models[-1]), failwith(f = runTest), payload = payload)

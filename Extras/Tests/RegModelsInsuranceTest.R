.libPaths(c(.libPaths(), "C:/Program Files/Alteryx/R-3.2.3/library"))
rev(.libPaths())
#' ### Regression Model Tests
library(testthat)
library(plyr)
options(testscript = TRUE)
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
options(testscript = NULL)
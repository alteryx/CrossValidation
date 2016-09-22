ayxPath <- "C:/Program Files/Alteryx/R-3.2.3/library"

.libPaths(c(.libPaths(), ayxPath))


library(testthat)

library(AlteryxPredictive)


source("C:/CrossValidation/Supporting_Macros/CrossValidation1.R")

data <- inputs$data 
models <- inputs$models 
modelNames <- inputs$modelNames

model1 <- models$Logit
model2 <- models$Tree

y_names <- sapply(models, getOneYVar)

## create set of models to purposely fail tests
bad_models <- c(models, bad_inputs$models)

test_that("getYvars function will return an error if models with different target variables", {
  expect_that(getYvars(data, bad_models), throws_error())
  
})

test_that("getYvars function returns y var when provided good data", {
  expect_that(getYvars(data, models), equals(data[[y_names[[1]]]]))
})

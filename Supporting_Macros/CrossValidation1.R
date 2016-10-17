#' ---
#' title: Cross Validation Macro
#' author: Bridget Toomey
#' ---
#' 
#' 

#' ### Read Configuration
#' ## DO NOT MODIFY: Auto Inserted by AlteryxRhelper ----
suppressWarnings(library(AlteryxPredictive))
config <- list(
  `classification` = radioInput('%Question.classification%' , TRUE),
  `displayGraphs` = checkboxInput('%Question.displayGraphs%' , TRUE),
  `modelType` = textInput("%Question.modelType%", NULL),
  `numberFolds` = numericInput('%Question.numberFolds%' , 5),
  `numberTrials` = numericInput('%Question.numberTrials%' , 3),
  `posClass` = textInput('%Question.posClass%'),
  `predFields` = listInput('%Question.predFields%'),
  `regression` = radioInput('%Question.regression%' , FALSE),
  `stratified` = checkboxInput('%Question.stratified%' , FALSE),
  `targetField` = dropdownInput('%Question.targetField%'),
  `seed` = numericInput('%Question.seed%', 1)
)
options(alteryx.wd = '%Engine.WorkflowDirectory%')
options(alteryx.debug = config$debug)
##----


library(plyr)
#' ### Defaults
#' These defaults are used when the R code is run outside Alteryx
if (!inAlteryx()){
  macroDirectory <- textInput('%Engine.WorkflowDirectory%', "Supporting_Macros")
  dataDir <- file.path(macroDirectory, "Data")
  defaults <- list(
    data = readRDS(file.path(dataDir, "data.rds")),
    models = readRDS(file.path(dataDir, "models.rds"))
  )
}


#' ### Inputs
#' 
#' This is a named list of all inputs that stream into the R tool.
inputs <- list(
  data = read.Alteryx2("#1", default = defaults$data),
  models = readModelObjects("#2", default = defaults$models)
)


##---- Inputs/Config Complete

#' ### Helper Functions
areIdentical <- function(v1, v2){
  identical(sort(v1), sort(v2))
}

#' Helper Functions For Tests
makePayload <- function(csvFile, modelsFile, targetVar){
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

#' ## Check predictor variables
#' 
#' Check if predictor variables in the models and input data are identical.
checkXVars <- function(inputs){
  numModels <- length(inputs$models)
  modelNames <- names(inputs$models)
  modelXVars <- lapply(inputs$models, getXVars2)
  dataXVars <- names(inputs$data)[which(names(inputs$data) %in% unlist(modelXVars))]
  errorMsg <- NULL
  if (numModels > 1) {
    for (i in 1:(numModels - 1)){
      mvars1 <- modelXVars[[i]]
      mvars2 <- modelXVars[[i + 1]]
      if (!areIdentical(mvars1, mvars2)){
        errorMsg <- paste("Models", modelNames[i] , "and", modelNames[i + 1],
                          "were created using different predictor variables.")
        stopMsg <- "Please ensure all models were created using the same predictors."
      } 
      else if (!all(mvars1 %in% dataXVars)){
        errorMsg <- paste("Model ", modelNames[i], 
                          "used predictor variables which were not contained in the input data.")
        stopMsg <- paste("Please ensure input data contains all the data",
                         "used to create the models and try again.")
      }
      if (!is.null(errorMsg)){
        AlteryxMessage2(errorMsg, iType = 2, iPriority = 3)
        stop.Alteryx2(stopMsg)
      }
    }
  } else {
    mvars1 <- modelXVars[[1]]
    if (!all(mvars1 %in% dataXVars)){
      errorMsg <- paste("Model ", modelNames[1], 
                        "used predictor variables which were not contained in the input data.")
      stopMsg <- paste("Please ensure input data contains all the data",
                       "used to create the models and try again.")
    }
    if (!is.null(errorMsg)){
      AlteryxMessage2(errorMsg, iType = 2, iPriority = 3)
      stop.Alteryx2(stopMsg)
    }
  }
}

#' Given a factor variable and a set of records in a fixed trial and fold,
#' return the list of classes not present in that trial and fold.
getMissingClasses <- function(currentClasses, currentRecords) {
  currentClasses[(!(currentClasses %in% currentRecords))]
}

#' For each factor variable, check to see if all levels are present in each fold. 
#' If not, warn the user.
#' 

checkFactorVars <- function(data, folds, config) {
  #All of the discrete variables will be some type of string in Alteryx. So they'll be read as factors, since stringsAsFactors is TRUE in read.Alteryx.
  factorVars <- data[,sapply(data, FUN = is.factor), drop = FALSE]
  #We only need to check if there's at least one factor variable. If all variables are continuous, we don't need to do anything.
  if (NCOL(factorVars) > 0) {
    for (k in 1:NCOL(factorVars)) {
      uniqueClasses <- unique(factorVars[,k])
      currentVar <- factorVars[,k]
      #We want to check if one of the folds on one of the trials is missing any classes.
      #If a class is missing from a fold, we output a warning suggesting that the user check their data/try to collect more data.
      #If a training set is missing a class, we output a fatal error telling the user they must ensure
      #that each training set contains all classes.
      for (i in 1: (config$numberTrials)) {
        for (j in 1:(config$numberFolds)) {
          currentTestRecords <- currentVar[unlist(folds[[i]][j])]
          currentTrainingRecords <- currentVar[unlist(folds[[i]][-j])]
          missingTestClasses <- getMissingClasses(currentClasses = uniqueClasses, currentRecords = currentTestRecords)
          missingTrainingClasses <- getMissingClasses(currentClasses = uniqueClasses, currentRecords = currentTrainingRecords)
          #testing if all classes are represented in trial i, fold j
          if (length(missingTestClasses) > 0) {
            currentColumnName <- colnames(factorVars)[k]
            if (length(missingTestClasses) > 1) {
              warningMessage1 <- paste0("Classes ", missingTestClasses, " were not present in variable ", currentColumnName, " of the test set.")
              warningMessage2 <- "It is recommended that you either check your data to ensure no records were mis-labeled or collect more data on these classes."
            } else {
              warningMessage1 <- paste0("Class ", missingTestClasses, " was not present in variable ", currentColumnName, " of the test set.")
              warningMessage2 <- "It is recommended that you either check your data to ensure no records were mis-labeled or collect more data on this class."
            }
            AlteryxMessage2(warningMessage1)
            AlteryxMessage2(warningMessage2)
          }
          #testing if all classes are represented in the training set when trial i, fold j is the test set. 
          #So the training set here is trial i, all folds except fold j.
          if (length(missingTrainingClasses) > 0) {
            currentColumnName <- colnames(factorVars)[k]
            if (length(missingTrainingClasses) > 1) {
              warningMessage1 <- paste0("Classes ", missingTrainingClasses, " were not present in variable ", currentColumnName," of the training set.")
              warningMessage2 <- "It is recommended that you either check your data to ensure no records were mis-labeled or collect more data on these classes."
              errorMessage <- "It is very difficult to create an accurate model when the training set is missing a class."
            } else {
              warningMessage1 <- paste0("Class ", missingTrainingClasses, " was not present in variable ", currentColumnName, " of the training set.")
              warningMessage2 <- "It is recommended that you either check your data to ensure no records were mis-labeled or collect more data on this class."
              errorMessage <- "It is very difficult to create an accurate model when the training set is missing classes."
            }
            AlteryxMessage2(warningMessage1)
            AlteryxMessage2(warningMessage2)
            AlteryxMessage2(errorMessage)
          }
        }
      }
    }
  }
}


#Create the list of cross-validation folds and output warnings/errors as appropriate
createFolds <- function(data, config) {
  target <- data[, 1]
  set.seed(config$seed)
  foldList <- generateCVRuns(labels = target, ntimes = config$numberTrials, nfold = config$numberFolds, stratified = config$stratified)
  checkFactorVars(data = data, folds = foldList, config = config)
  return(foldList)
}


#Get a single y variable using a model call. This function will be used as a helper function in getYvars, which obtains
#the Y variable from each model and checks them against each other and the Y variable from the input data.
getOneYVar <- function(model) {
  if (inherits(model, "naiveBayes")) {
    bayes_yvar <- model$yvars
    return(as.character(bayes_yvar))
  } else {
    return(as.character(formula(model$call))[2])
  }
}
#Check if response variable is the same in the pre-built model(s) and the input data.
#If so, output this variable.
getYvars <- function(data, models) {
  # Get the names of the target fields and make sure they are all same. If not,
  # throw an error.
  y_names <- sapply(models, getOneYVar)
  if (!all(y_names == y_names[1])) {
    stop.Alteryx2("More than one target variable are present in the provided models")
  } else if (!(y_names[1] %in% colnames(data))) {
    stop.Alteryx2("The target variable from the models is different than the target chosen in the configuration. Please check your configuration settings and try again.")
  }
  # get the target variable name
  y_name <- y_names[1]
  # Get the target variable
  return(data[[y_name]])
}

#In the 2-class classification case, get the positive class. Otherwise, do nothing.
getPosClass <- function(config, yVar) {

      #Use the function from the Model Comparison tool to get/set positive class:
      setPositiveClass <- function(tar_lev) {
        # Set the postive class for two-class classification.
        # The setPositiveClass function is only triggered if the user leaves the
        # question on positive class (target level) blank.
        #   1) if there's "yes/Yes/YES ..." in the target variable, then use "yes/Yes/YES"
        #   2) if there's "true/True/TRUE..." in the target variable, then use "true/True/TRUE"
        #   3) otherwise: use the first level by alphabetical order.
        #
        # Parameters:
        #   tar_lev: a vector of string
        #            the levels of the target variable.
        #
        # Returns:
        #   no_name: a string, the name of the positive class.
        
        yes_id <- match("yes", tolower(tar_lev))
        true_id <- match("true", tolower(tar_lev))
        if (!is.na(yes_id)) {
        return (tar_lev[yes_id])
      } else if (!is.na(true_id)) {
         return (tar_lev[true_id])
      } else {
        return (tar_lev[1])
      }
    }
    return(setPositiveClass(yVar))
}

adjustGbmModel <- function(model){
  method <- if (model$cv.folds > 1){
   "cv"
  } else if (model$train.function < 1){
    "test"
  } else {
    "OOB"
  }
  model$best.trees <- gbm.perf(model, method = method)
  return(model)
}



#' Given a model, a dataset and index of test cases, return actual and response
getActualandResponse <- function(model, data, testIndices, extras, mid){
  trainingData <- data[-testIndices,]
  testData <- data[testIndices,]
  testData <- matchLevels(testData, getXlevels(model))
  currentYvar <- getOneYVar(model)
  currentModel <- update(model, data = trainingData)
  if (inherits(currentModel, 'gbm')){
    currentModel <- adjustGbmModel(currentModel)
  }
  pred <- scoreModel(currentModel, new.data = testData)
  actual <- (extras$yVar)[testIndices]
  if (config$classification) {
    response <- gsub("Score_", "", names(pred)[max.col(pred)])
    d <- data.frame(response = response, actual = actual)
    return(cbind(d, pred))
  } else {
    response <- pred$Score
    return(data.frame(response = response, actual = actual))
  }
}

safeGetActualAndResponse <- failwith(NULL, getActualandResponse, quiet = TRUE)
#safeGetActualAndResponse <- getActualandResponse

#' 
getCrossValidatedResults <- function(inputs, allFolds, extras, config){
  function(mid, trial, fold){
    model <- inputs$models[[mid]]
    testIndices <- allFolds[[trial]][[fold]]
    out <- (safeGetActualAndResponse(model, inputs$data, testIndices, extras, mid))
    if (is.null(out)) {
      AlteryxMessage2(paste0("For model ", mid, " trial ", trial, " fold ", "the data could not be scored."), iType = 2, iPriority = 3)
    } else {
      out <- cbind(trial = trial, fold = fold, mid = mid, out)
    }
    return(out)
  }
}

getPkgListForModels <- function(models){
  modelClasses <- unlist(lapply(models, class))
  pkgMap = list(
    gbm = "gbm", rpart = "rpart", svm.formula = "e1071", svm = "e1071",
    naiveBayes = "e1071", svyglm = "survey", nnet.formula = "nnet",
    randomForest.formula = "randomForest", earth = "earth"
  )
  unique(unlist(pkgMap[modelClasses]))
}




#Get the necessary measures in the regression case
getMeasuresRegression <- function(outData, extras) {
  actual <- unlist(outData$actual)
  predicted <- unlist(outData$response)
  modelIndic <- outData$mid
  trialIndic <- outData$trial
  foldIndic <- outData$fold
  err <- actual - predicted
  rmse <- sqrt(mean(err*err))
  mae <- mean(abs(err))
  # When there are values near 0 in the target variable, it can lead to an attempt to divide by 0
  # In this case, use the weighted version.
  if (any(abs(actual) < 0.001)) {
    AlteryxMessage("The target variable contains values very close to 0 (-0.001, 0.001). WPE and WAPE are being used instead of MPE and MAPE.", iType = 2, iPriority = 3)
    mpe <- 100 * sum(err) / sum(actual)
    mape <- 100 * sum(abs(err)) / sum(actual)
  } else {
    mpe <- 100*mean(err/actual)
    mape <- 100*mean(abs(err/actual))
  }
  data.frame(
    cor = cor(predicted, actual), rmse = rmse, mae = mae, mpe= mpe, mape = mape
  )
}

#Get the necessary measures in the classification case
getMeasuresClassification <- function(outData, extras) {
  actual <- as.character(outData$actual)
  scoredData <- outData[,6:7]
  scoredOutput <- as.character(outData$response)
  posClass <- extras$posClass
  modelIndic <- unique(outData$mid)
  trialIndic <- unique(outData$trial)
  foldIndic <- unique(outData$fold)
  overallAcc <- sum(actual == scoredOutput)/length(actual)
  if (length(extras$levels) == 2) {
    true_y <- factor(TRUE*(actual == posClass)) # if levels are strings rather than TRUE/FALSE
    #We need to know which column of scoredData corresponds to the positive class in order to set up the needed intermediate steps for obtaining the AUC
    posClassCol <- which((extras$levels) == posClass)
    negClassCol <- which((extras$levels) != posClass)
    predictions <- scoredData[,posClassCol]
    predictionObj <- prediction(predictions = predictions, labels = actual)
    
    # =================================================================
    # Quick Reference:
    #       precision = tp / (tp + fp)
    #          recall = tp / (tp + fn)
    #             tpr = tp / (tp + fn)
    #             fpr = fp / (fp + tn)
    #              f1 = 2 * precision * recall / (precision + recall)
    # ==================================================================
    
    #     perf_acc <- performance(predictionObj, "acc", "cutoff")
    #     perf_lift <- performance(predictionObj, "lift", "rpp")
    #     perf_gain <- performance(predictionObj, "tpr", "rpp")
    #     perf_roc <- performance(predictionObj, "tpr", "fpr")
    #     perf_pr <- performance(predictionObj, "prec", "rec")
    actualPosIndic <- which(actual == posClass)
    nActualPos <- length(actualPosIndic)
    nCorrectPos <- sum(scoredOutput[actualPosIndic] == posClass)
    nPredPos <- sum(scoredOutput == posClass)
    precision <- nCorrectPos/nPredPos
    recall <- nCorrectPos/nActualPos
    F1 <- 2*(precision*recall)/(precision + recall)
    #F1 <- performance(predictionObj, "f")
    #F1 <- unlist(F1@y.values) # converting S4 class to scalar
    
    AUC <- performance(prediction.obj = predictionObj, measure = "auc")
    AUC <- unlist(AUC@y.values)
    #     rocrMeasures <- list(accuracy = perf_acc, lift = perf_lift, gain = perf_gain,
    #                          roc = perf_roc, pr = perf_pr, AUC = AUC, F1 = F1)
    percentClass1Right <- sum(scoredOutput[which(actual == (extras$levels)[1])] == (extras$levels)[[1]])/length(which(actual == (extras$levels)[1]))
    percentClass2Right <- sum(scoredOutput[which(actual == (extras$levels)[2])] == (extras$levels)[[2]])/length(which(actual == (extras$levels)[2]))
    outVec <- c(mid = modelIndic, trial = trialIndic, fold = foldIndic, Overall_Accuracy = overallAcc, Accuracy_Class_1 = percentClass1Right, Accuracy_Class_2 = percentClass2Right, F1 = F1, AUC = AUC)
    #outList <- list(outVec, rocrMeasures)
  } else {
    #Compute accuracy by class
    outVec <- vector(length = length((extras$levels)))
    for (l in 1:length((extras$levels))) {
      tempPred <- scoredOutput[actual == (extras$levels)[[l]]]
      nCorrect <- sum(tempPred == (extras$levels)[[l]])
      thisAcc <- nCorrect/sum(actual == (extras$levels)[[l]])
      outVec[l] <- thisAcc
      names(outVec)[l] <- paste0("Accuracy_Class_", l)
    }
    outVec <- c(mid = modelIndic, trial = trialIndic, fold = foldIndic, Overall_Accuracy = overallAcc, outVec)
    #outList <- list(outvec)
  }
  return(outVec)
}

#' ### Functions to Generate Output
#' 
generateConfusionMatrices <- function(outData, extras) {
  outvec <- vector(length = length(extras$levels))
  pasteClass <- function(nameOfClass) {
    paste0("Class_", nameOfClass)
  }
  names(outvec) <- sapply(X = (extras$levels), FUN = pasteClass, simplify = TRUE)
  for (i in 1:length(extras$levels)) {
    outvec[i] <- length(which((outData[,3+i]) == ((extras$levels)[i])))
  }
  return(c(mid = unique(outData$mid), trial = unique(outData$trial), fold = unique(outData$fold), Predicted_class = as.character(unique(outData$response)), outvec))
}

generateOutput3 <- function(data, extras, modelNames) {
  d <- ddply(data, .(trial, fold, mid, response), generateConfusionMatrices, 
    extras = extras
  )
  
  d$Model <- modelNames[as.numeric(d$mid)]
  d$Type <- rep.int('Classification', times = length(d$Model))
  d <- subset(d, select = -c(mid, response))
  d <- reshape2::melt(d, id = c('trial', 'fold', 'Model', 'Type', 'Predicted_class'))
  colnames(d) <- c('Trial', 'Fold', 'Model', 'Type', 'Predicted_class', 'Variable', 'Value')
  return(d)
}

generateOutput2 <- function(data, extras, modelNames) {
  fun <- if (is.null(extras$levels)) {
    getMeasuresRegression 
  } else {
    getMeasuresClassification
  }
  d <- ddply(data, .(trial, fold, mid), fun, extras = extras)
  d$Model <- modelNames[as.numeric(d$mid)]
  d <- subset(d, select = -c(mid))
  d <- reshape2::melt(d, id = c('trial', 'fold', 'Model'))
}

generateOutput1 <- function(inputs, config, extras){
  pkgsToLoad <- getPkgListForModels(inputs$models)
  for (pkg in pkgsToLoad) library(pkg, character.only = TRUE)
  allFolds <- extras$allFolds
  g <- expand.grid(
    mid = seq_along(inputs$models),
    trial = seq_along(allFolds),
    fold = seq_along(allFolds[[1]])
  )
  return(mdply(g, getCrossValidatedResults(inputs, allFolds, extras, config)))
}

computeBinaryMetrics <- function(pred_prob, actual, threshold){
  #Pred_prob gives the predicted probability of belonging to the positive class
  #Actual is true if the record belongs to the positive class and negative if not
  actualPosIndic <- which(actual == TRUE)
  actualNegIndic <- which(actual == FALSE)
  nActualPos <- length(actualPosIndic)
  thresholdedPredictions <- (pred_prob >= threshold)
  nCorrectPos <- sum(thresholdedPredictions[actualPosIndic])
  nPredPos <- sum(thresholdedPredictions)
  nPredNeg <- length(actual) - length(nPredPos)
  nCorrectNeg <- sum(1 - (thresholdedPredictions[-actualPosIndic]))
  overallAcc <- sum(thresholdedPredictions == actual)/length(actual)
  PosAcc <- length(intersect(which(thresholdedPredictions == TRUE), actualPosIndic))/length(actualPosIndic)
  NegAcc <- length(intersect(which(thresholdedPredictions == FALSE), actualNegIndic))/length(actualNegIndic)
  precision <- nCorrectPos/nPredPos
  recall <- nCorrectPos/nActualPos
  F1 <- 2*(precision*recall)/(precision + recall)
  tpr <- recall
  rpp <- nPredPos/length(pred_prob)
  lift <- tpr/rpp
  fpr <- (nPredPos - nCorrectPos)/length(actualNegIndic)
  pred <- prediction(predictions = pred_prob, labels = actual)
  auc <- performance(pred, "auc")
  auc <- unlist(auc@y.values)
  data.frame(threshold = threshold, recall = recall, F1 = F1, lift = lift, Rate_Pos_Predictions = rpp, True_Pos_Rate = tpr, False_Pos_Rate = fpr, Precision = precision)
}

generateDataForPlots <- function(d, extras, config){
  if ((length(extras$levels) == 2) && (config$classification)) {
    thresholds <- seq(0, 1, 0.05)
    ldply(thresholds, computeBinaryMetrics, 
          actual = ifelse(d$actual == extras$posClass, TRUE, FALSE), 
          pred_prob = d[[paste0('Score_', extras$posClass)]]
    )
  } else {
    data.frame(response = d$response, actual = d$actual)
  }
}


generateLabels <- function(plotData, config) {
  trials <- c()
  for (i in 1:length(unique(plotData$trial))) {
    trials <- c(trials, paste0("Trial ", unique(plotData$trial))[i])
  }
  models <- c()
  for (i in 1:length(unique(plotData$mid))) {
    models <- c(models, paste0("Model ", unique(plotData$model))[i])
  }
  list(trials = trials, models = models)
}

plotBinaryData <- function(plotData, config, modelNames) {
  labels <- generateLabels(plotData, config)
  modelVec <- modelNames[plotData$mid]
  trialVec <- paste0('Trial ', plotData$trial)
  plotData <- cbind(plotData, modelVec, trialVec)
  liftdf <- data.frame(Rate_positive_predictions = plotData$Rate_Pos_Predictions, lift = plotData$lift, fold = paste0("Fold", plotData$fold), 
                       models = plotData$modelVec, trial = plotData$trialVec)
  gaindf <- data.frame(Rate_positive_predictions = plotData$Rate_Pos_Predictions, True_Pos_Rate = plotData$True_Pos_Rate, fold = paste0("Fold", plotData$fold),
                       models = plotData$modelVec, trial = plotData$trialVec)
  prec_recalldf <- data.frame(recall = plotData$recall, precision = plotData$Precision, fold = paste0("Fold", plotData$fold),
                              models = plotData$modelVec, trial = plotData$trialVec)
  rocdf <- data.frame(False_Pos_Rate = plotData$False_Pos_Rate, True_Pos_Rate = plotData$True_Pos_Rate, fold = paste0("Fold", plotData$fold),
                      models = plotData$modelVec, trial = plotData$trialVec)
  
  liftPlotObj <- ggplot(data = liftdf, aes(x = Rate_positive_predictions, y = lift)) + facet_grid(models ~ trial) + 
    geom_line(aes(colour=fold)) + ggtitle("Lift curves") 
  gainPlotObj <- ggplot(data = gaindf, aes(x = Rate_positive_predictions, y = True_Pos_Rate)) + facet_grid(models ~ trial) + 
    geom_line(aes(colour=fold)) + ggtitle('Gain Charts')
  PrecRecallPlotObj <- ggplot(data = prec_recalldf, aes(x = recall, y = precision)) + facet_grid(models ~ trial) + 
    geom_line(aes(colour=fold)) + ggtitle('Precision and Recall Curves')
  ROCPlotObj <- ggplot(data = rocdf, aes(x = False_Pos_Rate, y = True_Pos_Rate)) + facet_grid(models ~ trial) +
    geom_line(aes(colour=fold)) + ggtitle('ROC Curves')
  AlteryxGraph2(liftPlotObj, nOutput = 4)
  AlteryxGraph2(gainPlotObj, nOutput = 4)
  AlteryxGraph2(PrecRecallPlotObj, nOutput = 4)
  AlteryxGraph2(ROCPlotObj, nOutput = 4)
}

plotRegressionData <- function(plotData, config, modelNames) {
  modelVec <- modelNames[plotData$mid]
  trialVec <- paste0('Trial ', plotData$trial)
  plotData <- cbind(plotData, modelVec, trialVec)
  plotdf <- data.frame(Actual = plotData$actual, Predicted = plotData$response, fold = paste0("Fold", plotData$fold), 
                       models = plotData$modelVec, trial = plotData$trialVec)
  plotObj <- ggplot(data = plotdf, aes(x = Actual, y = Predicted)) + facet_grid(models ~ trial) + 
    geom_line(aes(colour=fold)) + ggtitle("Predicted value vs actual values")
  AlteryxGraph2(plotObj, nOutput = 4)
}

# Helper Functions End ----

runCrossValidation <- function(inputs, config){
  checkInstalls(c("ROCR", "plyr", "TunePareto", "sm", "vioplot", "ggplot2"))
  library(ROCR)
  library("TunePareto")
  library("sm")
  library("vioplot")
  library("ggplot2")
  
  if (!is.null(config$modelType)){
    config$classification = config$modelType == "classification"
    config$regression = !config$classification
  }

  yVar <- getYvars(inputs$data, inputs$models)
  if ((config$classification) && (length(unique(yVar)) == 2)) {
    if (config$posClass == "") {
      config$posClass <- as.character(getPosClass(config, levels(yVar)))
    }
  }
  
  inputs$modelNames <- names(inputs$models)
  modelNames <- names(inputs$models)
  checkXVars(inputs)
  
  
  
  extras <- list(
    yVar = yVar,
    posClass = config$posClass,
    allFolds = createFolds(data = inputs$data, config = config),
    levels = if (config$classification) levels(yVar) else NULL
  )
  
  dataOutput1 <- generateOutput1(inputs, config, extras)
  preppedOutput1 <- data.frame(Trial = dataOutput1$trial, Fold = dataOutput1$fold, Model = modelNames[dataOutput1$mid],
                               Response = dataOutput1$response, Actual = dataOutput1$actual)
  write.Alteryx2(preppedOutput1, nOutput = 1)


  dataOutput2 <- generateOutput2(dataOutput1, extras, modelNames)
  write.Alteryx2(dataOutput2, nOutput = 2)

  if (config$classification) {
    confMats <- generateOutput3(dataOutput1, extras, modelNames)
    write.Alteryx2(confMats, 3)
  } else {
    #Provide garbage data that'll get filtered out on the Alteryx side.
    write.Alteryx2(data.frame(Trial = 1, Fold = 1, Model = 'model', Type = 'Regression', Predicted_class = 'no', Variable = "Classno", Value = 50), 3)
  }
  
  if (config$displayGraphs) {
    plotData <- ddply(dataOutput1, .(trial, fold, mid), generateDataForPlots, 
                      extras = extras, config = config
    )
    if (config$classification) {
      if (length(extras$levels) == 2) {
        print('binary case')
        plotBinaryData(plotData, config, modelNames)
      } else {
        #Generate an empty plot
        empty_df <- data.frame()
        emptyPlot <- ggplot(empty_df) + geom_point() + xlim(0, 1) + ylim(0, 1) + ggtitle("No plots available for >2 class classification")
        AlteryxGraph2(emptyPlot, nOutput = 4)
      }
    } else {
      print('regression case')
      plotRegressionData(plotData, config, modelNames)
    }
  }
}


if (is.null(getOption("testscript"))){
  runCrossValidation(inputs, config)
}

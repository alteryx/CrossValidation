#' Load Packages ----
library(AlteryxPredictive)
library(ROCR)
library(plyr)
library("TunePareto")
library("sm")
library("vioplot")



config <- list(
  `classification` = radioInput('%Question.classification%' , TRUE),
  `displayGraphs` = checkboxInput('%Question.displayGraphs%' , FALSE),
  `numberFolds` = numericInput('%Question.numberFolds%' , 5),
  `numberTrials` = numericInput('%Question.numberTrials%' , 3),
  `posClass` = textInput('%Question.posClass%'),
  `predFields` = listInput('%Question.predFields%'),
  `regression` = radioInput('%Question.regression%' , FALSE),
  `stratified` = checkboxInput('%Question.stratified%' , FALSE),
  `targetField` = dropdownInput('%Question.targetField%')
)
options(alteryx.wd = '%Engine.WorkflowDirectory%')
options(alteryx.debug = config$debug)

#' Given a factor variable and a set of records in a fixed trial and fold,
#' return the list of classes not present in that trial and fold.
getMissingClasses <- function(currentClasses, currentRecords) {
  currentClasses[(!(currentClasses %in% currentRecords))]
}

#' For each factor variable, check to see if all levels are present in each fold. 
#' If not, error out with an informative message.
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
  } else if (!(y_names[1] == colnames(data[,1, drop = FALSE]))) {
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

#' ### Defaults
#' 
#' These defaults are used when the R code is run outside Alteryx
macroDirectory <- textInput('%Engine.WorkflowDirectory%', "Supporting_Macros")
dataDir <- file.path(macroDirectory, "Data")
defaults <- list(
  data = readRDS(file.path(dataDir, "data.rds")),
  models = readRDS(file.path(dataDir, "models.rds"))
)

#' ### Inputs
#' 
#' This is a named list of all inputs that stream into the R tool.
inputs <- list(
  data = read.Alteryx2("#1", default = defaults$data),
  models = readModelObjects("#2", default = defaults$models)
)

inputs$modelNames = names(inputs$models)

yVar <- getYvars(inputs$data, inputs$models)

if ((config$classification) && (length(unique(yVar)) == 2)) {
  #For some reason, length(config$posClass) is still 1 even when posClass isn't given.
  #However, appending a single character to it when it's empty will result in a string of length 1
  if (length(paste0(config$posClass, "A")) == 1) {
  config$posClass <- getPosClass(config, yVar)
  }
}

extras <- list(
  yVar = colnames(inputs$data)[1],
  posClass = config$posClass,
  allFolds = createFolds(data = inputs$data, config = config)
)

#' Given a model, a dataset and index of test cases, return actual and response
getActualandResponse <- function(model, data, testIndices, extras){
  trainingData = data[-testIndices,]
  testData = data[testIndices,]
  currentModel <- update(model, data = trainingData)
  pred <- scoreModel(currentModel, new.data = testData)
  actual <- as.character(testData[[extras$yVar]])
  if (config$classification) {
    response <- gsub("Score_", "", names(pred)[max.col(pred)])
    return(data.frame(response = response, actual = actual, pred))
  } else {
    response <- pred
    return(data.frame(response = response, actual = actual))
  }
}

#' 
getCrossValidatedResults <- function(inputs, allFolds, extras){
  function(mid, trial, fold){
    model <- inputs$models[[mid]]
    testIndices <- allFolds[[trial]][[fold]]
    out <- (getActualandResponse(model, inputs$data, testIndices, extras))
    out <- cbind(trial = trial, fold = fold, mid = mid, out)
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


generateOutput3 <- function(inputs, config, extras){
  pkgsToLoad <- getPkgListForModels(inputs$models)
  for (pkg in pkgsToLoad) library(pkg, character.only = TRUE)
  allFolds <- extras$allFolds
  g <- expand.grid(
    mid = seq_along(inputs$models),
    trial = seq_along(allFolds),
    fold = seq_along(allFolds[[1]])
  )
  mdply(g, getCrossValidatedResults(inputs, allFolds, extras))
}

dataOutput3 <- generateOutput3(inputs, config, extras)

if (inAlteryx()) {
  write.Alteryx(dataOutput3, 3)
}

#Append an extra column that will contain trial, fold, and model ID concatenated
lastCol <- paste0(dataOutput3[,1], dataOutput3[,2], dataOutput3[,3])

#Get the necessary measures in the regression case
getMeasuresRegression <- function(outData) {
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
  c(as.character(modelIndic), trialIndic, foldIndic, cor(predicted, actual), rmse, mae, mpe, mape)
}

#Get the necessary measures in the classification case
getMeasuresClassification <- function(outData) {
  actual <- outData$actual
  scoredData <- outData[,6:7]
  scoredOutput <- outData$response
  #I know this isn't ideal, but I'm not sure how to get around it if we're using ddply (so we only want to have a single data.frame argument)
  posClass <- config$posClass
  modelIndic <- unique(outData$mid)
  trialIndic <- unique(outData$trial)
  foldIndic <- unique(outData$fold)
  myLevels <- levels(actual)
  overallAcc <- sum(actual == scoredOutput)/length(actual)
  if (length(myLevels) == 2) {
    true_y <- factor(TRUE*(actual == posClass)) # if levels are strings rather than TRUE/FALSE
    #We need to know which column of scoredData corresponds to the positive class in order to set up the needed intermediate steps for obtaining the AUC
    posClassCol <- which(myLevels == posClass)
    negClassCol <- which(myLevels != posClass)
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
    percentClass1Right <- sum(scoredOutput[which(actual == myLevels[1])] == myLevels[[1]])/length(which(actual == myLevels[1]))
    percentClass2Right <- sum(scoredOutput[which(actual == myLevels[2])] == myLevels[[2]])/length(which(actual == myLevels[2]))
    outVec <- c(mid = modelIndic, trial = trialIndic, fold = foldIndic, Overall_Accuracy = overallAcc, Accuracy_Class_1 = percentClass1Right, Accuracy_Class_2 = percentClass2Right, F1 = F1, AUC = AUC)
    #outList <- list(outVec, rocrMeasures)
  } else {
    #Compute accuracy by class
    outVec <- vector(length = length(myLevels))
    for (l in 1:length(myLevels)) {
      tempPred <- scoredOutput[actual == myLevels[[l]]]
      nCorrect <- sum(temppred == myLevels[[l]])
      thisAcc <- nCorrect/sum(actual == myLevels[[l]])
      outVec[l] <- thisAcc
      names(outVec)[l] <- paste0("Accuracy_Class_", l)
    }
    outVec <- c(mid = modelIndic, trial = trialIndic, fold = foldIndic, Overall_Accuracy = overallAcc, outVec)
    #outList <- list(outvec)
  }
  return(outVec)
}


generateOutput2 <- function(inputs) {
  if (config$regression) {
    return(ddply(.data = dataOutput3, .(trial, fold, mid), .fun = getMeasuresRegression))
  } else {
    return(ddply(.data = dataOutput3, .(trial, fold, mid), .fun = getMeasuresClassification))
  }
}

dataOutput2 <- generateOutput2(inputs)

if (inAlteryx()) {
  write.Alteryx(dataOutput2, 2)
}
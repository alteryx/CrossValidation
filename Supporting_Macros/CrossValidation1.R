#' ---
#' title: Cross Validation Macro
#' author: Bridget Toomey
#' ---
#' 
#' 
#' This is a utility function to check to see if the necessary packages are
#' installed and install them if they're not.

checkInstalls <- function(packages) {
  # See if the desired packages are installed, and install if they're not
  if (!all(packages %in% row.names(installed.packages()))) {
    # Use the IE based "Internet2" since it is most reliable for this action,
    # it will be switched back at the end
    setInternet2(use = TRUE)
    # Make sure the path to the users library is in place and create it if it
    # is not
    minor_ver <- strsplit(R.Version()$minor, "\\.")[[1]][1]
    R_ver <- paste(R.Version()$major, minor_ver, sep = ".")
    the_path <- paste0(normalizePath("~"), "\\R\\win-library\\", R_ver)
    # Create the user's personal folder if it doesn't already exist
    if (!dir.exists(the_path)) {
      dir.create(the_path, recursive = TRUE, showWarnings = FALSE)
    }
    # The set of possible repositories to use
    repos <- c("http://cran.revolutionanalytics.com", "https://cran.rstudio.com")
    # Select a particular repository
    repo <- sample(repos, 1)
    missingPackages <- packages[which(!(packages %in% row.names(installed.packages())))]
    install.packages(missingPackages, lib = the_path, repos = repo)
    setInternet2(use = FALSE)
  }
}
checkInstalls(c("AlteryxPredictive", "ROCR", "plyr", "TunePareto", "sm", "vioplot"))
#' ---

#' ### Read Configuration
#' ## DO NOT MODIFY: Auto Inserted by AlteryxRhelper ----
suppressWarnings(library(AlteryxPredictive))
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
##----

#' ### Defaults
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

##---- Inputs/Config Complete

#' ### Helper Functions
areIdentical <- function(v1, v2){
  identical(sort(v1), sort(v2))
}

#' ## Check predictor variables
#' 
#' Check if predictor variables in the models and input data are identical.
checkXVars <- function(inputs){
  numModels <- length(inputs$models)
  modelNames <- names(inputs$models)
  modelXVars <- lapply(inputs$models, getXVars)
  dataXVars <- names(inputs$data[,-1])
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
      } else if (!all(dataXVars %in% mvars1)){
        errorMsg <- paste("The input data contained variables not used in model", 
                          modelNames[i])
        stopMsg <- paste("Please be sure to select only the fields actually used as",
                         "predictors in the models and try again.")
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
    } else if (!all(dataXVars %in% mvars1)){
      errorMsg <- paste("The input data contained variables not used in model", 
                        modelNames[1])
      stopMsg <- paste("Please be sure to select only the fields actually used as",
                       "predictors in the models and try again.")
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
getActualandResponse <- function(model, data, testIndices, extras){
  trainingData = data[-testIndices,]
  testData = data[testIndices,]
  currentModel <- update(model, data = trainingData)
  if (inherits(currentModel, 'gbm')){
    currentModel <- adjustGbmModel(currentModel)
  }
  pred <- scoreModel(currentModel, new.data = testData)
  actual <- testData[[extras$yVar]]
  if (config$classification) {
    response <- gsub("Score_", "", names(pred)[max.col(pred)])
    return(data.frame(response = response, actual = actual, pred))
  } else {
    response <- pred$Score
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
  actual <- outData$actual
  scoredData <- outData[,6:7]
  scoredOutput <- outData$response
  #I know this isn't ideal, but I'm not sure how to get around it if we're using ddply (so we only want to have a single data.frame argument)
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
      nCorrect <- sum(temppred == (extras$levels)[[l]])
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
  return(c(mid = unique(outData$mid), trial = unique(outData$trial), fold = unique(outData$fold), Predicted_class = unique(outData$response), outvec))
}

generateOutput1 <- function(data, extras) {
  d <- ddply(data, .(trial, fold, mid, response), generateConfusionMatrices, 
    extras = extras
  )
  reshape2::melt(d, id = c('trial', 'fold', 'mid', 'response', 'Predicted_class'))
}

generateOutput2 <- function(data, extras) {
  fun <- if (is.null(extras$levels)) {
    getMeasuresRegression 
  } else {
    getMeasuresClassification
  }
  d <- ddply(data, .(trial, fold, mid), fun, extras = extras)
  reshape2::melt(d, id = c('trial', 'fold', 'mid'))
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

computeBinaryMetrics <- function(pred_prob, actual, threshold){
  #Pred_prob gives the predicted probability of belonging to the positive class
  #Actual is true if the record belongs to the positive class and negative if not
  actualPosIndic <- which(actual == TRUE)
  nActualPos <- length(actualPosIndic)
  thresholdedPredictions <- (pred_prob >= actual)
  nCorrectPos <- sum(thresholdedPredictions[actualPosIndic])
  nPredPos <- sum(thresholdedPredictions)
  overallAcc <- sum(thresholdedPredictions == actual)/length(actual)
  PosAcc <- sum(thresholdedPredictions == TRUE)/length(which(actual))
  NegAcc <- sum(thresholdedPredictions == FALSE)/sum(actual == FALSE)
  precision <- nCorrectPos/nPredPos
  recall <- nCorrectPos/nActualPos
  F1 <- 2*(precision*recall)/(precision + recall)
  probPredPos <- nPredPos/length(pred_prob)
  sizeIntersectPredAndActualPos <- length(intersect(which(actual == TRUE), which(pred_prob >= actual)))
  lift <- sizeIntersectPredAndActualPos/nActualPos
  rpp <- nPredPos/length(pred_prob)
  tpr <- nActualPos/length(pred_prob)
  fpr <- (nPredPos - nCorrectPos)/length(pred_prob)
  pred <- prediction(predictions = pred_prob, labels = actual)
  auc <- performance(pred, "auc")
  auc <- unlist(auc@y.values)
  data.frame(threshold = threshold, recall = recall, F1 = F1, lift = lift, Rate_Pos_Predictions = rpp, True_Pos_Rate = tpr, False_Pos_Rate = fpr)
}

generateDataForPlots <- function(d, extras, config){
  if (config$classification) {
    if (length(extras$levels) == 2) {
      thresholds <- seq(0, 1, 0.05)
      ldply(thresholds, computeBinaryMetrics, 
            actual = ifelse(d$actual == extras$posClass, TRUE, FALSE), 
            pred_prob = d[[paste0('Score_', extras$posClass)]]
      )
    } else {
      data.frame(response = d$response, actual = d$actual)
    }
  }
}



# Helper Functions End ----

runCrossValidation <- function(inputs, config){
  
  
  library(ROCR)
  library(plyr)
  library("TunePareto")
  library("sm")
  library("vioplot")

  yVar <- getYvars(inputs$data, inputs$models)
  if ((config$classification) && (length(unique(yVar)) == 2)) {
    if (config$posClass == "") {
      config$posClass <- as.character(getPosClass(config, levels(yVar)))
    }
  }
  
  inputs$modelNames = names(inputs$models)
  checkXVars(inputs)
  
  extras <- list(
    yVar = colnames(inputs$data)[1],
    posClass = config$posClass,
    allFolds = createFolds(data = inputs$data, config = config),
    levels = if (config$classification) levels(yVar) else NULL
  )
  
  # FIXME: clean up hardcoded values
  dataOutput3 <- generateOutput3(inputs, config, extras)
  write.Alteryx2(dataOutput3[,1:5], nOutput = 3)
  #print(head(dataOutput3[,1:5]))
  
  dataOutput2 <- generateOutput2(dataOutput3, extras)
  write.Alteryx2(dataOutput2, nOutput = 2)
  #print(head(dataOutput2))
  
  if (config$classification) {
    confMats <- generateOutput1(dataOutput3, extras)
    write.Alteryx2(confMats, 1)
  }
  
  ddply(dataOutput3, .(trial, fold, mid), generateDataForPlots, extras = extras, config = config)
}

runCrossValidation(inputs, config)

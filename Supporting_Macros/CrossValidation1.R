#' ---
#' title: Cross Validation Macro
#' author: Bridget Toomey
#' ---

#' ## Read
#' 
#' The first step is to read configuration and inputs that stream in from
#' Alteryx. For this code to be portable, we need to provide defaults that will
#' be used when the R code is not run inside an Alteryx workflow.
#'
#' ### Configuration
## DO NOT MODIFY: Auto Inserted by AlteryxRhelper ----
library(AlteryxPredictive)
print("loaded library")
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
}

#' Given a factor variable and a set of records in a fixed trial and fold,
#' return the list of classes not present in that trial and fold.
getMissingClasses <- function(currentClasses, currentRecords) {
  currentClasses[(!(currentClasses %in% currentRecords))]
}

#' For each factor variable, check to see if all levels are present in each fold. 
#' If not, error out with an informative message.
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
              errorMessage <- "It is impossible to create an accurate model when the training set is missing a class."
            } else {
              warningMessage1 <- paste0("Class ", missingTrainingClasses, " was not present in variable ", currentColumnName, " of the training set.")
              warningMessage2 <- "It is recommended that you either check your data to ensure no records were mis-labeled or collect more data on this class."
              errorMessage <- "It is impossible to create an accurate model when the training set is missing classes."
            }
            AlteryxMessage2(warningMessage1)
            AlteryxMessage2(warningMessage2)
            stop.Alteryx2(errorMessage)
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

#Get the necessary measures in the classification case
getMeasuresClassification <- function(actual, scoredData, scoredOutput, posClass, modelIndic, trialIndic, foldIndic, modelNames) {
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
    
    perf_acc <- performance(predictionObj, "acc", "cutoff")
    perf_lift <- performance(predictionObj, "lift", "rpp")
    perf_gain <- performance(predictionObj, "tpr", "rpp")
    perf_roc <- performance(predictionObj, "tpr", "fpr")
    perf_pr <- performance(predictionObj, "prec", "rec")
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
    rocrMeasures <- list(accuracy = perf_acc, lift = perf_lift, gain = perf_gain,
                         roc = perf_roc, pr = perf_pr, AUC = AUC, F1 = F1)
    percentClass1Right <- sum(scoredOutput[which(actual == myLevels[1])] == myLevels[[1]])/length(which(actual == myLevels[1]))
    percentClass2Right <- sum(scoredOutput[which(actual == myLevels[2])] == myLevels[[2]])/length(which(actual == myLevels[2]))
    outVec <- c(as.character(modelNames[modelIndic]), trialIndic, foldIndic, overallAcc, percentClass1Right, percentClass2Right, F1, AUC)
    outList <- list(outVec, rocrMeasures)
  } else {
    #Compute accuracy by class
    outVec <- vector(length = length(myLevels))
    for (l in 1:length(myLevels)) {
      tempPred <- scoredOutput[actual == myLevels[[l]]]
      nCorrect <- sum(temppred == myLevels[[l]])
      thisAcc <- nCorrect/sum(actual == myLevels[[l]])
      outVec[l] <- thisAcc
    }
    outVec <- c(as.character(modelNames[modelIndic]), trialIndic, foldIndic, overallAcc, outVec)
    outList <- list(outvec)
  }
  return(outList)
}

#Get them in the regression case
getMeasuresRegression <- function(actual, predicted, modelIndic, trialIndic, foldIndic, modelNames) {
  actual <- unlist(actual)
  predicted <- unlist(predicted)
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
  c(as.character(modelNames[modelIndic]), trialIndic, foldIndic, cor(predicted, actual), rmse, mae, mpe, mape)
}

generateHists <- function(fitMeasures, config, nLevels) {
  fitMeasures <- data.matrix(fitMeasures)
  if (config$regression) {
    return(list(Correlation = hist(fitMeasures[,1], breaks = "Sturges"), RMSE = hist(fitMeasures[,2], breaks = "Sturges"), MAE = hist(fitMeasures[,3], breaks = "Sturges"), MPE = hist(fitMeasures[,4], breaks = "Sturges"), MAPE = hist(fitMeasures[,5], breaks = "Sturges")))
  } else if (nLevels == 2) {
    return(list(Overall_Accuracy = hist(fitMeasures[,1], breaks = "Sturges"), Accuracy_Class1 = hist(fitMeasures[,2], breaks = "Sturges"), Accuracy_Class2 = hist(fitMeasures[,3], breaks = "Sturges"), F1 = hist(fitMeasures[,4], breaks = "Sturges"), AUC = hist(fitMeasures[,5], breaks = "Sturges")))
  } else {
    tempList <- list(Overall_Accuracy = hist(fitMeasures[,1], breaks = "Sturges"))
    for (m in 1:nLevels) {
      tempList[[m+1]] <- hist(fitMeasures[,(1+m)], breaks = "Sturges")
      names(tempList)[m+1] <- paste0("Class_", m)
    }
    return(tempList)
  }
}


#' ##Install additional packages (if needed)
#'Make sure the necessary packages that don't already come with the Predictive install are installed.
#'Install the needed packages if they are not available
needed <-  c("ROCR", "TunePareto") %in% row.names(installed.packages())
if (!all(needed)) {
  if (!needed[1]) {
    install.packages("ROCR", repos = "https://cran.rstudio.com")
  }
  if (!needed[2]) {
    install.packages("TunePareto", repos = "https://cran.rstudio.com")
  }
}

#'Load the needed libraries
suppressWarnings(library("ROCR"))
suppressWarnings(library("TunePareto"))
suppressWarnings(library("sm"))
suppressWarnings(library("vioplot"))


cvResults <- function(config, data, models, modelNames) {
  #histList will store the histogram objects
  histList <- list()
  #Do the unserialization.
  allFolds <- createFolds(data = data, config = config)
  # Get the set of model classes
  #We only take the first class of each model, since that's all we need to determine the necessary libraries and ensure that all of the input models
  #are allowable types.
  #TRY TO MOVE OTHER LIBRARY CALLS DOWN HERE SO THEY'RE ALL TOGETHER!
  modelClasses <- vector(mode = "character", length = length(models))
  for (i in 1:length(models)) {
    fullCurrentClasses <- class(models[[i]])
    modelClasses[i] <- fullCurrentClasses[[1]]
  }
  # Load the needed packages given the set of models
  if (any(modelClasses == "gbm")) {
    suppressWarnings(library("gbm"))
  }
  if (any(modelClasses == "rpart")) {
    suppressWarnings(library("rpart"))
  }
  if (any(modelClasses == "svm.formula" | modelClasses == "naiveBayes")) {
    suppressWarnings(library("e1071"))
  }
  if (any(modelClasses == "svyglm")) {
    suppressWarnings(library("survey"))
  }
  if (any(modelClasses == "nnet.formula")) {
    suppressWarnings(library("nnet"))
  }
  if (any(modelClasses == "randomForest.formula")) {
    suppressWarnings(library("randomForest"))
  }
  if (any(modelClasses == "earth")) {
    suppressWarnings(library("earth"))
  }
  
  # Remove models from the input that are not supported
  classTest <- modelClasses %in% c("gbm", "rpart", "svm.formula", "glm", "svyglm", "nnet.formula", "randomForest.formula", "earth", "lm", "naiveBayes")
  if (!all(classTest)) {
    # Warn the user about which models are being removed
    if (sum(as.numeric(!classTest)) == 1) {
      AlteryxMessage(paste("The model", models$Name[!classTest], "is not a supported type, and has been removed."))
    } else {
      bad_models <- paste(models$Name[!classTest], collapse = ", ")
      AlteryxMessage(paste("The models", bad_models, "are not a supported type, and have been removed."))
    }
    models <- models[[classTest]]
  }
  #Only checking X vars for now, not defining them. We need to define them separately for each model in case the different models
  #have them in a different order
  checkXVars(inputs = inputs)
  yVar <- getYvars(data = data, models = models)
  #Need to get levels for classification case and the positive class in binomial case
  if (config$classification) {
    myLevels <- attr(yVar, "levels")
    if (length(myLevels) < 2) {
      stop.Alteryx2("The target variable has fewer than two levels! Please ensure that the target has at least 2 levels and try again.")
    } else if (length(myLevels) == 2) {
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
      posClass <- setPositiveClass(myLevels)
      
      #Initialize list that will hold the information needed for the 2-class specific plots
      listOutMeasures <- as.list(vector(length = (length(models) * (config$numberTrials))))
    }
  } 
  
  #Initialize table of Actual|Model 1|...|Model n|Trial Number
  fullTable <- matrix(0, nrow = NROW(data) * (config$numberTrials), ncol = (length(models) + 2))
  nameVector <- vector(mode = "character", length = length(models))
  for (i in 1:length(models)) {
    nameVector[i] <- paste0(modelNames[i], " Model")
  }
  nameVector <- c("Actual", nameVector, "Trial_Number")
  colnames(fullTable) <- nameVector
  #Initialize the table that will have the results from getMeasuresRegression or getMeasuresClassification.
  #This table will have 6 columns in the regression case, 6 in the 2-class case, and n+2 in the n class case when n>2.
  if (config$regression) {
    nMeasuresCols <- 8
    colNames <- c("Model", "Trial", "Fold", "Correlation", "RMSE", "MAE", "MPE", "MAPE")
  } else if (length(myLevels) == 2) {
    nMeasuresCols <- 8
    colNames <- c("Model", "Trial", "Fold", "Overall_Accuracy", paste0("Accuracy_Class_", myLevels[[1]]), paste0("Accuracy_Class_", myLevels[[2]]), "F1", "AUC")
  } else {
    nMeasuresCols <- 4 + length(myLevels)
    pasteNames <- function(names) {
      paste0("Class_", names)
    }
    colNames <- sapply(X = unlist(myLevels), FUN = pasteNames, simplify = TRUE)
    colNames <- c("Model", "Trial", "Fold", "Overall Accuracy", colNames)
  }
  getMeasuresOut <- as.data.frame(matrix(vector(length = (nMeasuresCols * length(models) * (config$numberTrials) * (config$numberFolds))), ncol = nMeasuresCols))
  colnames(getMeasuresOut) <- colNames
  
  for (i in 1:length(models)) {
    #Make sure the predictors in the data are in the same order as they are in the current model
    fullData <- data[,getXVars(models[[i]])]
    #Now we have target|Predictor 1|...|Predictor n
    fullData <- cbind(data[,1], fullData)
    colnames(fullData)[1] <- colnames(data)[1]
    #We can initialize currentCall here, since we're not evaluating it until the innermost loop, where the.data is updated.
    currentBigModel <- models[[i]]
    currentCall <- currentBigModel$call
    #We need to intialize the row of fullTable to 1 everytime we increment i, since that means we're moving on to the next column of the table
    startRow <- 1
    #The number of rows in the table from previous models is the total number of records * the number of previous trials * the number of previous models
    #Indexing these rows here makes it easier to index the trials within the j loop.
    
    for (j in 1:(config$numberTrials)) {
      #Clear out the old values of fullScoredData and fullActualData
      #I THINK WE ONLY NEED THESE IN THE BINOMIAL CLASSIFICATION CASE! COME BACK AND CHECK!
      if (i == 1) {
        #Append trial number indicator
        #Only need to update during the first model
        rowStartCurrentTrial <- 1 + (j - 1) * NROW(data)
        rowEndCurrentTrial <- NROW(data) * j
        fullTable[rowStartCurrentTrial:rowEndCurrentTrial,NCOL(fullTable)] <- j
      }
      
      currentFolds <- allFolds[[j]]
      #When we increment j, we need to re-increment startRow to start on the next section of fullTable that corresponds to the current trial.
      startRow <- 1 + ((j - 1) * NROW(data))
      for (k in 1:(config$numberFolds)) {
        #Define the first and last rows of fullTable that we'll be updating for this value of k
        if (k > 1) {
          #In the k > 1 case, we need update the value of startRow to be 1 more than the previous value of endRow
          startRow <- endRow + 1
        }
        endRow <- startRow + length(currentFolds[[k]]) - 1
        currentTestRows <- currentFolds[[k]]
        #As of August 2016, our new style guidelines encourage the use of camel case over dots. But defining the training data as the.data allows us to use 
        #the model's call directly, since all of the existing Predictive tools use the.data in their calls. 
        #However, if we re-name the.data as something else as part of the Predictive Refresh, we will also have to rename accordingly here.
        the.data <- fullData[-currentTestRows, ]
        
        #New approach: Use update
        trainingData <- fullData[-currentTestRows, ]
        testData <- fullData[currentTestRows, ]
        currentTrueData <- testData[, 1]
        
        #We need to define the "Actual" column of fullTable when i == 1
        if (i == 1) {
          if (config$classification) {
            fullTable[startRow:endRow, 1] <- myLevels[currentTrueData]
          } else {
            fullTable[startRow:endRow, 1] <- currentTrueData
          }
        }
        #Now that we've updated the.data and testData, we can define currentModel
        #currentModel <- eval(currentCall)
        wd = "%Engine.WorkflowDirectory%"
        #         saveRDS(models[[i]], file.path(wd, "nb.RDS"))
        #         saveRDS(trainingData, file.path(wd, "trainingdata.RDS"))
        currentModel <- update(models[[i]], data = trainingData)
        #The Alteryx Boosted Model tool adds an attribute called best.trees to the gbm model object. 
        #best.trees is the optimal number of trees according to the user selected test (either cross-validation, a single test/training split, or out of bag estimates).
        #We need to add the attribute best.trees to this.model in order for scoreModel to work correctly, since scoreModel expects best.trees and will error without it.
        if ("gbm" %in% class(currentModel)) {
          if (currentModel$cv.folds > 1) {
            assess.string <- 'gbm.perf(currentModel, method = "cv")'
          }
          else if (currentModel$train.fraction < 1) {
            assess.string <- 'gbm.perf(currentModel, method = "test")'
          }
          else {
            assess.string <- 'gbm.perf(currentModel, method = "OOB")'
          }
          currentModel$best.trees <- eval(parse(text = assess.string))
        }
        scoredData <- scoreModel(currentModel, new.data = testData)
        if (config$classification) {
          #In the classification case, we need to take the class with the highest probability
          #So taking the maximum of each row gives us the desired result
          #For now, I have a suspicion that we might want to create a giant table of all the scoredData pieces together. 
          #Thus, I'm creating another new variable rather than re-defining scoredData.
          scoredOutput <- apply(scoredData, MARGIN = 1, FUN = which.max)
          scoredOutput <- myLevels[scoredOutput]
        } else {
          #scoreModel always outputs a column of predicted values in the regression case
          scoredOutput <- scoredData
        }
        #Since the first column is the actual data, we need to update column i+1 instead of column i
        fullTable[startRow:endRow, i + 1] <- unlist(scoredOutput)
        if ((config$classification) && length(myLevels == 2)) {
          tempOutMeasures <- getMeasuresClassification(actual = currentTrueData, scoredData = scoredData, scoredOutput = scoredOutput, posClass = posClass, modelIndic = i, trialIndic = j, foldIndic = k, modelNames = modelNames)
          outMeasures <- tempOutMeasures[[1]]
          if (j == 1) {
            listOutMeasures[[(((i - 1) * config$numberTrials) + j)]] <- tempOutMeasures
          } else {
            listOutMeasures[[(((i - 1) * config$numberTrials) + j)]] <- c(listOutMeasures[[(((i - 1) * config$numberTrials) + j)]], tempOutMeasures)
          }
          
        } else {
          outMeasures <- getMeasuresRegression(actual = currentTrueData, predicted = scoredOutput, modelIndic = i, trialIndic = j, foldIndic = k, modelNames = modelNames)
        }
        currentOMRow <- (i - 1) * (config$numberTrials) * (config$numberFolds) + (j - 1) * config$numberFolds + k
        getMeasuresOut[currentOMRow,] <- outMeasures
      }
    }
    #Get the rows belonging to the current model
    currentRows <- which(getMeasuresOut[,1] == modelNames[i])
    histList[[i]] <- generateHists(fitMeasures = getMeasuresOut[currentRows,4:NCOL(getMeasuresOut)], config = config, nLevels = length(myLevels))
    #invisible(dev.off())
  }
  write.Alteryx2(getMeasuresOut, 2)
  write.Alteryx2(as.data.frame(fullTable), 3)
  #write.Alteryx(as.data.frame(fullTable), 3)
  #Now that we have the appropriate table, it's time to move on to the different fit measures that vary by problem type.
  #Check if the problem is classification or regression.
  if (config$classification) {
    
    #Create the confusion matrices in "one giant matrix" using key-value pairs
    confMat <- matrix(0, nrow = (length(models) * (config$numberTrials) * length(myLevels)), ncol = 3 + length(myLevels))
    #Name the columns
    columnNames <- vector(mode = "character", length = length(myLevels))
    for (i in 1:length(myLevels)) {
      columnNames[i] <- paste0("Class ", myLevels[[i]])
    }
    columnNames <- c("Model", "Trial","Predicted_class", columnNames)
    colnames(confMat) <- columnNames
    #Define the first two columns
    for (i in 1: length(models)) {
      #The number of rows that have already been done by previous models is (i - 1)*length(myLevels) * (config$numberTrials)
      confMatstartRow <- (i - 1)*length(myLevels) * (config$numberTrials) + 1
      confMatendRow <- i * length(myLevels) * (config$numberTrials)
      confMat[confMatstartRow:confMatendRow, 1] <- as.character((inputs$modelNames)[i])
      for (j in 1: (config$numberTrials)) {
        #The number of rows that have been done by different trials in the same level is confMatstartRow + (j - 1) * length(myLevels) - 1
        trialStart <- confMatstartRow + (j - 1) * length(myLevels)
        trialEnd <- trialStart  + length(myLevels) - 1
        confMat[trialStart:trialEnd, 2] <- j
        #We only want the rows of the table corresponding to trial j and the columns corresponding to the actual result and the current model
        fullTableRows <- which(fullTable[,NCOL(fullTable)] == j)
        subTable <- fullTable[fullTableRows,c(1, 1 + i)]
        for (k in 1:length(myLevels)) {
          #Iterate over the records that actually belong to class k
          modelKRows <- which(subTable[,1] == myLevels[[k]])
          #Define the vector containing all records from model i, trial j that were actually in class k
          currentActuals <- subTable[modelKRows,2]
          for (l in 1:length(myLevels)) {
            #How many records actually in class k are predicted as class l?
            confMat[(trialStart + l  - 1),(k + 3)] <- length(which(currentActuals == myLevels[[l]]))
            #Update the Predicted_class column as well
            confMat[(trialStart + l  - 1),3] <- myLevels[[l]]
          }
        }
      }
    }
    #Write out the confusion matrix to output #1
    dfConfMat <- as.data.frame(confMat)
    write.Alteryx2(dfConfMat, 1)
  } 
  if ((config$classification) && (length(myLevels) == 2)) {
    print("listOutMeasures is:")
    print(listOutMeasures)
  }
  #write.Alteryx(as.data.frame(measuresMatrix), 2)
  #saveRDS(histList, file.path(wd, "histograms.RDS"))
  #AlteryxGraph(4)
  #lapply(histList, sapply, plot)
  #Unfortunately I think the nested *apply approach won't work because the labels need to be updated correctly for each round
  for (i in 1:length(histList)) {
    for (j in 1:length(histList[[1]])) {
      plot((histList[[i]])[[j]], main = paste0("Histogram of ", names(histList[[i]])[[j]], " ", modelNames[i]), xlab = names(histList[[i]])[[j]])
    }
  }
  
  if (config$regression) {
    xvals <- fullTable[,1]
    for (i in 1:NROW(models)) {
      yvals <- fullTable[,i+1]
      for (j in 1:config$numberTrials) {
        xvals_current_trial <- xvals[which(fullTable[,NCOL(fullTable)] == j)]
        yvals_current_trial <- yvals[which(fullTable[,NCOL(fullTable)] == j)]
        main_title <- paste("Plot of Actuals and Predicted values for Model", modelNames[i], " Trial ", j)
        plot(c(min(c(xvals, yvals)), max(c(xvals, yvals))), c(min(c(xvals, yvals)), max(c(xvals, yvals))), type = "n", xlab = "Actual", ylab = "Predicted", main = main_title)
        points(xvals_current_trial, yvals_current_trial, col = j)
      }
    }
  } else if (length(myLevels) == 2) {
    print("listOutMeasures is:")
    print(listOutMeasures)
  }
  #invisible(dev.off())
}


cvResults(config = config, data = inputs$data, models = inputs$models, modelNames = inputs$modelNames)

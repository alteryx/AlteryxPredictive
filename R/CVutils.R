#' Cross-validation for Decision Tree
#'


#Helper Functions
# Checks whether two vectors have the same elements
#'
#' @param v1 a vector
#' @param v2 a vector
#' @return Boolean indicating whether the two vectors are equal
areIdentical <- function(v1, v2){
  identical(sort(v1), sort(v2))
}

#' Given a factor variable and a set of records in a fixed trial and fold,
#' return the list of classes not present in that trial and fold.
#'
#' @param currentClasses a vector of unique class names
#' @param currentRecords a vector of the classes in the current fold.
#' @return vector of the classes that are missing from the current fold.
getMissingClasses <- function(currentClasses, currentRecords) {
  currentClasses[(!(currentClasses %in% currentRecords))]
}

#' For each factor variable, check to see if all levels are present in each fold.
#' If not, warn the user.
#'
#' @param data a data.frame with the data used to generate the models
#' @param folds a list of record id's in each fold in each trial (list of lists)
#' @param config a list of configuration information
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


#' Create the list of cross-validation folds
#'
#' @param data the data.frame used to create the models
#' @param config a list of configuration information
#' @param set_seed boolean of whether to (re)set seed
#' @param seed integer value of random seed
#' @return list of record ID's. Each element is the record ID's of the folds for a given trial.
#' @import TunePareto
createFolds <- function(data, config, set_seed = TRUE, seed = NULL) {
  target <- data[, 1]
  if (set_seed) {
    set.seed(seed)
  }
  foldList <- TunePareto::generateCVRuns(labels = target, ntimes = config$numberTrials, nfold = config$numberFolds, stratified = config$stratified)
  checkFactorVars(data = data, folds = foldList, config = config)
  return(foldList)
}

#' Check if response variable is the same in the pre-built model(s) and the input data.
#' If so, output this variable.
#'
#' @param data incoming data
#' @param models model(s) to extract Y vars from
#' @return y variable
#' @export
getYvars <- function(data, models) {
  # Get the names of the target fields and make sure they are all same. If not,
  # throw an error.
  y_names <- sapply(models, getYVar)
  if (!all(y_names == y_names[1])) {
    stop.Alteryx2("More than one target variable are present in the provided models")
  } else if (!(y_names[1] %in% colnames(data))) {
    stop.Alteryx2("The target variable from the models is different than the target chosen in the configuration. Please check your configuration settings and try again.")
  }
  # get the target variable name
  y_name <- y_names[1]
  # Get the target variable
  return(list(y_col = data[[y_name]], y_name = y_name))
}

#' Get the postive class for two-class classification, choosing the
#' positive class to the less-common class when all else fails.
#'
#' @param target_levels a vector of strings with the target variable's levels
#' @return string - name of positive class
#' @export
getPositiveClass <- function(target_levels) {
  # no/yes
  yes_id <- match("yes", tolower(target_levels))
  if (!is.na(yes_id)) {
    return (target_levels[yes_id])
  }
  # false/true
  true_id <- match("true", tolower(target_levels))
  if(!is.na(true_id)) {
    return (target_levels[true_id])
  }
  # 0/1
  one_id <- match("1", target_levels)
  if(!is.na(one_id)) {
    return (target_levels[one_id])
  }
  # Nothing obvious, so assume less-common class is positive.
  first_class <- target_levels[1]
  second_class <- target_levels[which(target_levels != first_class)[1]]
  if (length(which(target_levels == first_class)) > length(which(target_levels == second_class))) {
    #First_class is larger, so second_class is the positive class
    return (second_class)
  } else {
    return (first_class)
  }
}

# Given a model, a dataset and index of test cases, return actual and response
#' @import C50 rpart glmnet
#' @importFrom stats update
#' @importFrom survey svyglm
getActualandResponse <- function(model, data, testIndices, extras, mid, config){
  if(class(model) == "rpart" || class(model) == "C5.0" || any(class(model) == "glm")) {
    trainingData <- data[-testIndices,]
    testData <- data[testIndices,]
    testData <- matchLevels(testData, getXlevels(model))
    currentYvar <- getYVar(model)
    if (inherits(model, "C5.0")) {
      weights_v <- trainingData[[config$`select.weights`]]
      currentModel <- C50Update(model, trainingData, currentYvar, config, weight_vec = weights_v)
    } else if (inherits(model, 'svyglm')){
      ### this seemingly useless if statement is very necessary
      ### best guess is there is some strange environment doings in svyglm
      if (config$Link == "complementary log-log" || config$Link == "cloglog"){
        currentModel <- update(
          object = model,
          data = trainingData,
          design = model$survey.design,
          # for consistency with original model
          family = quasibinomial("cloglog")
        )
      } else {
        currentModel <- update(
          object = model,
          data = trainingData,
          design = model$survey.design,
          # for consistency with original model
          family = quasibinomial(config$Link)
        )
      }
    } else{
      currentModel <- update(model, data = trainingData)
    }
    pred <- scoreModel(currentModel, new.data = testData)
    actual <- (extras$yVar)[testIndices]
    recordID <- (data[testIndices,])$recordID
    if (config$classification) {
      response <- gsub("Score_", "", names(pred)[max.col(pred)])
      d <- data.frame(recordID = recordID, response = response, actual = actual)
      return(cbind(d, pred))
    } else {
      response <- pred$Score
      return(data.frame(recordID = recordID, response = response, actual = actual))
    }
  } else {
    trainingData <- data[-testIndices,]
    testData <- data[testIndices,]
    testData <- matchLevels(testData, getXlevels(model))
    currentYvar <- extras$y_name
    #Check if the model is Naive Bayes and lacking a Laplace parameter.
    #If so, set the Laplace parameter to 0 and warn the user.
    #     if (inherits(model, "naiveBayes")) {
    #       currentModel <- naiveBayesUpdate(model, trainingData, currentYvar)
    #     } else
    if ((inherits(model, "cv.glmnet")) || (inherits(model, "glmnet"))) {
      #Ideally, it would be more efficient to convert the x df to a matrix earlier so that
      #this conversion wouldn't be necessary with every trial/fold. However, the code assumes
      #that we're dealing with a df in many other places. This are could be ripe for refactoring
      #in the future.
      weights_v <- if(config$`Use Weights`) trainingData[[config$`Weight Vec`]] else NULL
      y_vec <- trainingData[[currentYvar]]
      trainingData_noyvar <- trainingData[, !(colnames(trainingData) %in% currentYvar), drop = FALSE]
      trainingData_noyvar <- df2NumericMatrix(
        x = trainingData_noyvar,
        filtering_message = "Non-numeric variables are among the predictors. They are now being removed.",
        convertVectorToDataFrame = TRUE
      )
      #No need to call df2NmericMatrix on testData, since scoreModel calls df2NumericMatrix with glmnet models.
      currentModel <- glmnetUpdate(model, trainingData_noyvar, y_vec, config, weight_vec = weights_v)
    } else if (inherits(model, "lm")){
      if (config$`Use Weights`) {
        # WORKAROUND
        # The assign() statement below moves the token ‘getActualandResponse’ to the global environment, where the update() function can find it.
        # Otherwise, something inside update() isn’t finding ‘getActualandResponse’ on its environment search path.
        #assign(x = 'trainingDatagetActualandResponse403', value = trainingData, envir = globalenv())
        my_envir <- environment()
        lapply(
          X = 1:ncol(trainingData),
          FUN = function(i){
            assign(
              x = names(trainingData)[i],
              value = trainingData[,i],
              envir = my_envir
            )
          }
        )
        currentModel <- update(model, formula. = makeFormula(getXVars(model), currentYvar), data = environment(), weights = trainingData$`Weight Vec`)
      } else {
        currentModel <- update(model, formula. = makeFormula(getXVars(model), currentYvar), data = trainingData)
      }
    }
    # if (inherits(currentModel, 'gbm')){
    #   currentModel <- adjustGbmModel(currentModel)
    # }
    pred <- scoreModel(currentModel, new.data = testData)
    actual <- (extras$yVar)[testIndices]
    recordID <- (data[testIndices,])$recordID
    if (config$classification) {
      response <- gsub("Score_", "", names(pred)[max.col(pred)])
      d <- data.frame(recordID = recordID, response = response, actual = actual)
      return(cbind(d, pred))
    } else {
      response <- pred$Score
      return(data.frame(recordID = recordID, response = response, actual = actual))
    }
    return(data.frame(recordID = recordID, response = response, actual = actual))
  }
}
#' @import plyr
safeGetActualAndResponse <- plyr::failwith(NULL, getActualandResponse, quiet = FALSE)

getCrossValidatedResults <- function(inputs, allFolds, extras, config){
  function(mid, trial, fold){
    model <- inputs$models[[mid]]
    testIndices <- allFolds[[trial]][[fold]]
    out <- (safeGetActualAndResponse(model, inputs$data, testIndices, extras, mid, config))
    if (is.null(out)) {
      AlteryxMessage2(paste0("For model ", mid, " trial ", trial, " fold ", fold, " the data could not be scored."), iType = 2, iPriority = 3)
    } else {
      out <- cbind(trial = trial, fold = fold, mid = mid, out)
    }
    return(out)
  }
}

#Get the necessary measures in the regression case
#' @importFrom stats cor
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
    AlteryxMessage2("The target variable contains values very close to 0 (-0.001, 0.001). WPE and WAPE are being used instead of MPE and MAPE.", iType = 2, iPriority = 3)
    mpe <- 100 * sum(err) / sum(actual)
    mape <- 100 * sum(abs(err)) / sum(actual)
  } else {
    mpe <- 100*mean(err/actual)
    mape <- 100*mean(abs(err/actual))
  }
  data.frame(
    Correlation = cor(predicted, actual), RMSE = rmse, MAE = mae, MPE= mpe, MAPE = mape
  )
}


#' Get the necessary measures in the classification case
#'
#' @param outData scored data used to obtain the measures
#' @param extras list of miscellaneous information
#' @return outvec a vector of results
#' @importFrom ROCR prediction
getMeasuresClassification <- function(outData, extras) {
  actual <- as.character(outData$actual)
  scoredData <- outData[,7:8]
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
    predictionObj <- ROCR::prediction(predictions = predictions, labels = actual)

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
    AUC <- performance(prediction.obj = predictionObj, measure = "auc")
    AUC <- unlist(AUC@y.values)
    percentClass1Right <- sum(scoredOutput[which(actual == (extras$levels)[1])] == (extras$levels)[[1]])/length(which(actual == (extras$levels)[1]))
    percentClass2Right <- sum(scoredOutput[which(actual == (extras$levels)[2])] == (extras$levels)[[2]])/length(which(actual == (extras$levels)[2]))
    outVec <- c(
      mid = modelIndic,
      trial = trialIndic,
      fold = foldIndic,
      Accuracy_Overall = overallAcc,
      Accuracy_Class_1 = percentClass1Right,
      Accuracy_Class_2 = percentClass2Right,
      F1 = F1,
      AUC = AUC
    )
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
    outVec <- c(mid = modelIndic, trial = trialIndic, fold = foldIndic, Accuracy_Overall = overallAcc, outVec)
  }
  return(outVec)
}

# Functions to Generate Output
#' @import reshape2
generateConfusionMatrices <- function(outData, extras) {
  outvec <- vector(length = length(extras$levels))
  pasteClass <- function(nameOfClass) {
    paste0("Class_", nameOfClass)
  }
  names(outvec) <- sapply(X = (extras$levels), FUN = pasteClass, simplify = TRUE)
  for (i in 1:length(extras$levels)) {
    outvec[i] <- length(which((outData$actual) == ((extras$levels)[i])))
  }
  return(c(mid = unique(outData$mid), trial = unique(outData$trial), fold = unique(outData$fold), Predicted_class = as.character(unique(outData$response)), outvec))
}

generateOutput3 <- function(data, extras, modelNames) {
  d <- plyr::ddply(data, c("trial", "fold", "mid", "response"), generateConfusionMatrices,
                   extras = extras
  )
  d$Model <- modelNames[as.numeric(d$mid)]
  d$Type <- rep.int('Classification', times = length(d$Model))
  d$mid <- NULL
  d$response <- NULL
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
  d <- plyr::ddply(data, c("trial", "fold", "mid"), fun, extras = extras)
  d$Model <- modelNames[as.numeric(d$mid)]
  d$mid <- NULL
  return(d)
}

#' @import plyr
#' @import rpart C50 glmnet
generateOutput1 <- function(inputs, config, extras){
  allFolds <- extras$allFolds
  g <- expand.grid(
    mid = seq_along(inputs$models),
    trial = seq_along(allFolds),
    fold = seq_along(allFolds[[1]])
  )
  return(mdply(g, getCrossValidatedResults(inputs, allFolds, extras, config)))
}


#' Get the necessary measures in the binary classification case
#'
#' @param pred_prob vector of predicted probabilities
#' @param actual vector of actual results
#' @param threshold a double between 0 and 1 (current probability threshold)
#' @return a data.frame with results
#' @importFrom ROCR prediction performance
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
  pred <- ROCR::prediction(predictions = pred_prob, labels = actual)
  auc <- ROCR::performance(pred, "auc")
  auc <- unlist(auc@y.values)
  data.frame(threshold = threshold, recall = recall, F1 = F1, lift = lift, Rate_Pos_Predictions = rpp, True_Pos_Rate = tpr, False_Pos_Rate = fpr, Precision = precision)
}

generateDataForPlotsDTree <- function(d, extras, config){
  if (config$classification) {
    if (length(extras$levels) == 2) {
      thresholds <- seq(0, 1, 0.05)
      plyr::ldply(thresholds, computeBinaryMetrics,
                  actual = ifelse(d$actual == extras$posClass, TRUE, FALSE),
                  pred_prob = d[[paste0('Score_', extras$posClass)]]
      )
    } else{
      data.frame()
    }
  } else {
    data.frame(response = d$response, actual = d$actual)
  }
}

generateDataForPlotsLinReg <- function(d, extras, config){
  data.frame(response = d$response, actual = d$actual)
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

#' @import ggplot2
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

  liftPlotObj <- ggplot2::ggplot(data = liftdf, aes_string(x = "Rate_positive_predictions", y = "lift")) +
    ggplot2::geom_smooth(aes_string(colour="models")) + ggplot2::ggtitle("Lift curves")
  gainPlotObj <- ggplot2::ggplot(data = gaindf, aes_string(x = "Rate_positive_predictions", y = "True_Pos_Rate")) +
    ggplot2::geom_smooth(aes_string(colour="models")) + ggplot2::ggtitle('Gain Charts')
  PrecRecallPlotObj <- ggplot2::ggplot(data = prec_recalldf, aes_string(x = "recall", y = "precision")) +
    ggplot2::geom_smooth(aes_string(colour="models")) + ggplot2::ggtitle('Precision and Recall Curves')
  ROCPlotObj <- ggplot2::ggplot(data = rocdf, aes_string(x = "False_Pos_Rate", y = "True_Pos_Rate")) +
    ggplot2::geom_smooth(aes_string(colour="models")) + ggplot2::ggtitle('ROC Curves')
  AlteryxGraph2(liftPlotObj, nOutput = 4)
  AlteryxGraph2(gainPlotObj, nOutput = 4)
  AlteryxGraph2(PrecRecallPlotObj, nOutput = 4)
  AlteryxGraph2(ROCPlotObj, nOutput = 4)
}
#' @import ggplot2
plotRegressionData <- function(plotData, config, modelNames) {
  modelVec <- modelNames[plotData$mid]
  trialVec <- paste0('Trial ', plotData$trial)
  plotData <- cbind(plotData, modelVec, trialVec)
  plotdf <- data.frame(Actual = plotData$actual, Predicted = plotData$response, fold = paste0("Fold", plotData$fold),
                       models = plotData$modelVec, trial = plotData$trialVec)
  plotObj <- ggplot2::ggplot(data = plotdf, aes_string(x = "Actual", y = "Predicted")) +
    ggplot2::geom_smooth(aes_string(colour="models")) + ggplot2::ggtitle("Predicted value vs actual values")
  AlteryxGraph2(plotObj, nOutput = 4)
}

#' Check predictor variables
#' Check if predictor variables in the models and input data are identical.
#' @param inputs list of inputs
checkXVars <- function(inputs){
  numModels <- length(inputs$models)
  modelNames <- names(inputs$models)
  modelXVars <-  lapply(inputs$models, getXVars)
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

glmnetUpdate <- function(model, trainingData_noyvar, y_vec, config, weight_vec = NULL) {
  predictors <- trainingData_noyvar[,getXVars(model)]
  if (ncol(predictors) < 2) {
    stop.Alteryx2(
      paste0(
        "Regularization requires at least two numeric predictors. ",
        "Please  switch to a non-regularized model, or use more predictors. "
      )
    )
  }
  response <- y_vec
  model_w_call <- if (config$internal_cv) {
    model$glmnet.fit
  } else {
    model
  }
  if (config$`Use Weights`) {
    currentModel <- update(model_w_call, x = predictors, y = response, weights = weight_vec)
  } else {
    #currentModel <- update(model, formula. = makeFormula(getXVars(model), currentYvar), data = trainingData)
    currentModel <- update(model_w_call, x = predictors, y = response)
  }
  currentModel$xvars <- colnames(predictors)
  currentModel$lambda_pred <- if (config$internal_cv) {
    if (config$lambda_1se) {
      model$lambda.1se
    } else {
      model$lambda.min
    }
  } else {
    config$lambda_no_cv
  }
  if ((inherits(model, "lognet")) || (inherits(model$glmnet.fit, "lognet"))) {
    currentModel$ylevels <- levels(y_vec)
  }
  return(currentModel)
}

C50Update <- function(model, trainingData, currentYvar, config, weight_vec = NULL) {
  var_names <- getNamesFromOrdered(names(trainingData), config$`use.weights`)
  if (config$`use.weights`) {
    currentModel <- update(model, formula. = makeFormula(getXVars(model), currentYvar),
                           data = trainingData, weights = weight_vec)
  } else {
    currentModel <- update(model, formula. = makeFormula(getXVars(model), currentYvar), data = trainingData)
  }
  currentModel$yvar <- var_names$y
  currentModel$xlevels <- lapply(X = trainingData[var_names$x], FUN = levels)
  currentModel$ylevels <- levels(trainingData[[var_names$y]])
  return(currentModel)
}

getPkgListForModels <- function(models){
  modelClasses <- unlist(lapply(models, class))
  pkgMap = list(
    gbm = "gbm", rpart = "rpart", svm.formula = "e1071", svm = "e1071",
    naiveBayes = "e1071", svyglm = "survey", nnet.formula = "nnet",
    randomForest.formula = "randomForest", earth = "earth", glmnet = "glmnet",
    elnet = "glmnet", cv.glmnet = "glmnet", lognet = "glmnet"
  )
  unique(unlist(pkgMap[modelClasses]))
}

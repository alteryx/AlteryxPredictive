#' Score function
#'
#' @param mod.obj model object
#' @param new.data new data to score
#' @param score.field name given to the score field
#' @param ... additional arguments
#' @export
#' @author Ramnath Vaidyanathan, Dan Putler, Bridget Toomey
#' @rdname scoreModel
scoreModel <- function(mod.obj, new.data, score.field = "Score", ...) {
  UseMethod('scoreModel')
}


#' @param os.value oversampling value
#' @param os.pct oversampling percent
#' @param pred.int whether to generate prediction intervals
#' @param int.vals interval values
#' @param log.y whether to report y on the log scale
#' @export
#' @rdname scoreModel
scoreModel.default <- function(mod.obj, new.data, score.field = "Score",
    os.value = NULL, os.pct = NULL, ...){
  target.value <- os.value
  new.data <- matchLevels(new.data, getXlevels(mod.obj))
  y.levels <- getYlevels(mod.obj, new.data)
  if (class(mod.obj) == "earth" && is.null(mod.obj$glm.list)) {
    stop.Alteryx2("Spline Models that did not use a GLM family cannot be scored")
  }
  if (is.null(y.levels)) {
    if(inherits(mod.obj, c("nnet.formula", "rpart", "svm"))){
      scores <- data.frame(score = as.vector(predict(mod.obj, newdata = new.data)))
    } else {
      if (class(mod.obj)[1] == "gbm") {
        scores <- data.frame(score = as.vector(predict(mod.obj, newdata = new.data, type = "response", n.trees = mod.obj$best.trees)))
      } else {
        scores <- data.frame(score = as.vector(predict(mod.obj, newdata = new.data, type = "response")))
      }
    }
    names(scores) <- score.field
  } else {
    if (!is.null(os.value)) {
      if (length(y.levels) != 2) {
        AlteryxRDataX::AlteryxMessage("Adjusting for the oversampling of the target is only valid for a binary categorical variable, so the predicted probabilities will not be adjusted.", iType = 2, iPriority = 3)
        scores <- data.frame(predProb(mod.obj, newdata = new.data))
      } else {
        sample.pct <- samplePct(mod.obj, os.value, new.data)
        wr <- sample.pct/os.pct
        wc <- (100 - sample.pct)/(100 - os.pct)
        pred.prob <- predProb(mod.obj, new.data)[ , (1:2)[y.levels == os.value]]
        adj.prob <- (pred.prob/wr)/(pred.prob/wr + (1 - pred.prob)/wc)
        if (y.levels[1] == target.value) {
          scores <- data.frame(score1 = adj.prob, score2 = 1 - adj.prob)
        } else {
          scores <- data.frame(score1 = 1 - adj.prob, score2 = adj.prob)
        }
      }
    } else {
      scores <- data.frame(predProb(mod.obj, new.data))
    }
    names(scores) <- paste(score.field, "_", y.levels, sep = "")
  }
  scores
}

#' @export
scoreModel.glm <- scoreModel.default

#' @export
scoreModel.svyglm <- scoreModel.default

#' @export
scoreModel.negbin <- scoreModel.default


#' @export
#' @rdname scoreModel
scoreModel.lm <- function(mod.obj, new.data, score.field = "Score",
     pred.int = FALSE, int.vals = NULL, log.y = FALSE, ...) {
  if (pred.int) {
    score <- as.data.frame(predict(mod.obj, newdata = new.data, level = 0.01*int.vals, interval = "predict"))
    if (log.y) {
      score$fit <- exp(score$fit)*(sum(exp(mod.obj$residuals))/length(mod.obj$residuals))
      score$lwr <- exp(score$lwr)*(sum(exp(mod.obj$residuals))/length(mod.obj$residuals))
      score$upr <- exp(score$upr)*(sum(exp(mod.obj$residuals))/length(mod.obj$residuals))
    }
    scores <- eval(parse(text = paste("data.frame(",score.field, "_fit = score$fit, ", score.field, "_lwr = score$lwr, ", score.field, "_upr = score$upr)", sep = "")))
  } else {
    score <- predict(mod.obj, newdata = new.data)
    if (log.y) {
      # The condition below checks to see if there are predicted values that
      # would imply machine infinity when expotentiated. If this is the case
      # a warning is given, and the smearing estimator is not applied. NOTE:
      # to make this code work nicely in non-Alteryx environments, the
      # AlteryxRDataX::AlteryxMessage call would need to be replaced with a message call
      if (max(score) > 709) {
        AlteryxRDataX::AlteryxMessage("The target variable does not appear to have been natural log transformed, no correction was applied.", iType = 2, iPriority = 3)
      } else {
        score <- exp(score)*(sum(exp(mod.obj$residuals))/length(mod.obj$residuals))
      }
    }
    scores <- eval(parse(text = paste("data.frame(", score.field, " = score)")))
  }
  scores
}

#' @export
#' @rdname scoreModel
scoreModel.rxLogit <- function(mod.obj, new.data, score.field = "Score",
    os.value = NULL, os.pct = NULL, ...) {
  new.data <- matchLevels(new.data, mod.obj$xlevels)
  pred.prob <- RevoScaleR::rxPredict(mod.obj, data = new.data, type = "response", predVarNames = "pred.prob")$pred.prob
  if (!is.null(os.value)) {
    target.value <- os.value
    num.target <- mod.obj$yinfo$counts[mod.obj$yinfo$levels == target.value]
    num.total <- sum(mod.obj$yinfo$counts)
    sample.pct <- 100*num.target / num.total
    wr <- sample.pct/os.pct
    wc <- (100 - sample.pct)/(100 - os.pct)
    if (mod.obj$yinfo$levels == target.value) {
      apr <- ((1 - pred.prob)/wr)/((1 - pred.prob)/wr + pred.prob/wc)
      scores <- data.frame(score1 = apr, score2 = 1 - apr)
    } else {
      adj.prob <- (pred.prob/wr)/(pred.prob/wr + (1 - pred.prob)/wc)
      scores <- data.frame(score1 = 1 - adj.prob, score2 = adj.prob)
    }
  } else {
    scores <- data.frame(score1 = 1 - pred.prob, score2 = pred.prob)
  }
  names(scores) <- eval(parse(text = paste('c("', score.field, '_', mod.obj$yinfo$levels[1], '", "', score.field, '_', mod.obj$yinfo$levels[2], '")', sep="")))
  scores
}

#' @export
#' @rdname scoreModel
scoreModel.rxGlm <- function(mod.obj, new.data, score.field = "Score", ...) {
  scores <- RevoScaleR::rxPredict(mod.obj, data = new.data, type = "response", predVarNames = "score")$score
  names(scores) <- score.field
  scores
}


#' @export
#' @rdname scoreModel
scoreModel.rxLinMod <- function(mod.obj, new.data, score.field = "Score", pred.int = FALSE, int.vals = NULL, log.y = FALSE, ...) {
  if (pred.int) {
    scores <- RevoScaleR::rxPredict(mod.obj, data = new.data, computeStdErrors = TRUE, interval = "prediction", confLevel = 0.01*int.vals, type = "response")
    scores <- scores[,-2]
    if (log.y)
      for (i in 1:3)
        scores[,i] <- exp(scores[[i]])*mod.obj$smearing.adj
    names(scores) <- paste(score.field, "_", c("fit", "lwr", "upr"), sep = "")
  } else {
    scores <- RevoScaleR::rxPredict(mod.obj, data = new.data, type = "response", predVarNames = "score")$score
    if (log.y) {
      if (is.null(mod.obj$smearing.adj)) {
        AlteryxRDataX::AlteryxMessage("The target variable does not appear to have been natrual log transformed, no correction was applied.", iType = 2, iPriority = 3)
      } else {
        scores <- exp(scores)*mod.obj$smearing.adj
      }
    }
  }
  scores
}

#' @export
#' @rdname scoreModel
scoreModel.rxDTree <- function(mod.obj, new.data, score.field, os.value = NULL,
    os.pct = NULL, ...) {
  new.data <- matchLevels(new.data, mod.obj$xlevels)
  # Classification trees
  if (!is.null(mod.obj$yinfo)) {
    scores <- RevoScaleR::rxPredict(mod.obj, data = new.data, type = "prob")
    if (class(mod.obj) == "rxDForest")
      scores <- scores[, -(ncol(scores))]
    if (!is.null(os.value)) {
      if (ncol(scores) != 2) {
        AlteryxRDataX::AlteryxMessage("Adjusting for the oversampling of the target is only valid for a binary categorical variable, so the predicted probabilities will not be adjusted.", iType = 2, iPriority = 3)
      } else {
        target.value <- os.value
        target.loc <- 2
        if (mod.obj$yinfo$levels[1] == target.value) {
          target.loc = 1
        }
        pred.prob <- scores[[target.loc]]
        num.target <- mod.obj$yinfo$counts[mod.obj$yinfo$levels == target.value]
        num.total <- sum(mod.obj$yinfo$counts)
        sample.pct <- 100*num.target / num.total
        wr <- sample.pct/os.pct
        wc <- (100 - sample.pct)/(100 - os.pct)
        if (mod.obj$yinfo$levels[1] == target.value) {
          apr <- ((1 - pred.prob)/wr)/((1 - pred.prob)/wr + pred.prob/wc)
          scores <- data.frame(score1 = apr, score2 = 1 - apr)
        } else {
          adj.prob <- (pred.prob/wr)/(pred.prob/wr + (1 - pred.prob)/wc)
          scores <- data.frame(score1 = 1 - adj.prob, score2 = adj.prob)
        }
      }
    }
    names(scores) <- paste(score.field, "_", mod.obj$yinfo$levels)
  } else { # Regression trees
    scores <- RevoScaleR::rxPredict(mod.obj, data = new.data, predVarNames = "score")$score
  }
  scores
}

#' @export
#' @rdname scoreModel
scoreModel.rxDForest <- scoreModel.rxDTree

#' @export
#' @rdname scoreModel
scoreModel.elnet <- function(mod.obj, new.data, score.field = "Score", ...) {
  #The code in the score tool has already subsetted the columns of the original
  #data to be scored, so there's no need to subset in that case.
  #However, we need to perform the subsetting and column ordering in case of future tools
  #that might use scoreModel. Unfortunately, glmnet isn't smart enough to order the columns
  #correctly in the predict function if they're provided in the wrong order.
  used_x_vars <- getXVars(mod.obj)
  new.data <- df2NumericMatrix(
    x = new.data,
    filtering_message = "Non-numeric variables are among the predictors. They are now being removed.",
    convertVectorToDataFrame = TRUE
  )
  if (!all(used_x_vars %in% colnames(new.data))) {
    missing_x_vars <- used_x_vars[!(used_x_vars %in% colnames(new.data))]
    if (length(missing_x_vars) == 1) {
      AlteryxPredictive::stop.Alteryx2(paste0("The incoming data stream is missing
                                              the variable ", missing_x_vars, ". Please make
                                              sure you provide this variable and try again."))
    } else {
      AlteryxPredictive::stop.Alteryx2(paste0("The incoming data stream is missing
                                              the variables ", missing_x_vars, ". Please make
                                              sure you provide these variables and try again."))
    }
  }
  used_data <- new.data[,used_x_vars]
  requireNamespace('glmnet')
  score <- predict(object = mod.obj, newx = used_data, s = mod.obj$lambda_pred)
  score <- as.data.frame(score)
  names(score) <- score.field
  return(score)
}

#' @export
#' @rdname scoreModel
scoreModel.lognet <- function(mod.obj, new.data, score.field = "Score",
                                 os.value = NULL, os.pct = NULL, ...) {
  used_x_vars <- getXVars(mod.obj)
  new.data <- df2NumericMatrix(
    x = new.data,
    filtering_message = "Non-numeric variables are among the predictors. They are now being removed.",
    convertVectorToDataFrame = TRUE
  )
  target.value <- os.value
  y.levels <- getYlevels(mod.obj)
  if (!all(used_x_vars %in% colnames(new.data))) {
    missing_x_vars <- used_x_vars[!(used_x_vars %in% colnames(new.data))]
    if (length(missing_x_vars) == 1) {
      AlteryxPredictive::stop.Alteryx2(paste0("The incoming data stream is missing
                                              the variable ", missing_x_vars, ". Please
                                              make sure you provide this variable and try again."))
    } else {
      AlteryxPredictive::stop.Alteryx2(paste0("The incoming data stream is missing
                                              the variables ", missing_x_vars, ". Please
                                              make sure you provide these variables and try again."))
    }
  }
  used_data <- new.data[,used_x_vars]
  requireNamespace('glmnet')
  if (!is.null(os.value)) {
    if (length(y.levels) != 2) {
      AlteryxMessage2("Adjusting for the oversampling of the target is only valid for a binary
                      categorical variable, so the predicted probabilities will not be adjusted.", iType = 2, iPriority = 3)
      scores <- predict(object = mod.obj, newx = used_data, s = mod.obj$lambda_pred, type = 'response')
      #Note that the predict.glmnet documentation says that only the probability of the second class is produced
      #So we need to take 1 - that result and set the first column to that
      scores <- data.frame(cbind((1 - scores), scores))
    } else {
      sample.pct <- samplePct(mod.obj, os.value, new.data)
      wr <- sample.pct/os.pct
      wc <- (100 - sample.pct)/(100 - os.pct)
      pred.prob <- predict(object = mod.obj, newx = used_data, s = mod.obj$lambda_pred, type = 'response')
      pred.prob <- as.data.frame(cbind((1 - pred.prob), pred.prob))
      pred.prob <- pred.prob[ , (1:2)[y.levels == os.value]]
      adj.prob <- (pred.prob/wr)/(pred.prob/wr + (1 - pred.prob)/wc)
      if (y.levels[1] == target.value) {
        scores <- data.frame(score1 = adj.prob, score2 = 1 - adj.prob)
       } else {
         scores <- data.frame(score1 = 1 - adj.prob, score2 = adj.prob)
      }
     }
  } else {
    scores <- predict(object = mod.obj, newx = used_data, s = mod.obj$lambda_pred, type = 'response')
    scores <- data.frame(cbind((1 - scores), scores))
  }
  names(scores) <- paste(score.field, "_", y.levels, sep = "")
  return(scores)
}

#' @export
#' @rdname scoreModel
scoreModel.cv.glmnet <- function(mod.obj, new.data, score.field = "Score",
                              os.value = NULL, os.pct = NULL, ...) {
  if (inherits(mod.obj$glmnet.fit, 'lognet')) {
    return(scoreModel.lognet(mod.obj, new.data, score.field = "Score",
                      os.value = NULL, os.pct = NULL, ...))
  } else {
    scoreModel.elnet(mod.obj, new.data, score.field = "Score",
                     os.value = NULL, os.pct = NULL, ...)
  }
}




#Note: When doing this for logistic regression, I'll need to update to differentiate between
#elnet and lognet types. I can test whether mod.obj$glmnet.fit inherits elnet.

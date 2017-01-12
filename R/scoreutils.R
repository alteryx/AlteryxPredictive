################################################################################
# S3 Class Version of the Scoring code                                         #
#                                                                              #
# This was done for two reasons: (1) to simplify the code in the Score macro   #
# which was becoming to difficult to maintain and (2) providing an ability for #
# other macros (e.g., Model Comparison) to take advantage of the same code     #
# used in the Score macro                                                      #
#                                                                              #
# Authors: Ramnath Vaidyanthan and Dan Putler                                  #
################################################################################
## Helper Functions ----
#' Extract predictor variables from an R model call object
#'
#' @param x model object
#' @export
getXVars <- function(x){
  UseMethod('getXVars')
}

getXVarsFromCall <- function(x){
  strsplit(as.character(formula(x))[3], "\\s*\\+\\s*")[[1]]
}

#' @export
getXVars.default <- function(x) {
  the.call <- x$call
  if(class(the.call) != "call") stop.Alteryx2("The argument was not a call object.")
  xvars <- getXVarsFromCall(x)
  xvars <- xvars[xvars != "-1"]
  chkNewLine <- function(string) {
    if (substr(string, 1, 1) == "\n")
      string <- trim.blanks(substr(string, 2, nchar(string)))
    string
  }
  xvars <- sapply(xvars, chkNewLine)
  names(xvars) <- NULL
  xvars
}

#' @export
getXVars.naiveBayes <- function(x) {
  x$xvars
}

#' @export
getXVars.svm.formula <- getXVars.naiveBayes
#The component xvars was added to glmnet model objects
#in the linearregutils code, so we can also extract it using
#$xvars.
#' @export
getXVars.glmnet <- getXVars.naiveBayes
#' @export
getXVars.cv.glmnet <- getXVars.naiveBayes

#' Remove non numeric elements from a list
#'
#' @param ll list
#' @export
noZeroLevels <- function(ll){Filter(Negate(is.numeric), ll)}


#' Remove factors with null levels from a list
#'
#' @param ll list of factor levels.
#' @export
noNullLevels <- function(ll){Filter(Negate(is.null), ll)}

#' Match levels of factors in original data with new data
#'
#' Coerce the levels in new data factors to exactly match the levels
#' of factors in the original data, and is needed for Revo ScaleR models
#' @param nd new data
#' @param ol old factor levels
#' @export
matchLevels <- function(nd, ol) {
  # Address the non-standard way randomForest returns xlevel values
  check.ol <- sapply(ol, is.numeric)
  ol[check.ol] <- NULL
  # if the model does appear to have factors then sort out the levels
  if (!(is.null(ol) || length(ol) == 0)) {
    factor.test <- sapply(nd, is.factor)
    the.factors <- names(nd)[factor.test]
    ol.names <- names(ol)
    ol.names <- ol.names[!is.na(ol.names)]
    if (!all(ol.names %in% the.factors))
      stop.Alteryx2("There are factor variables in the model that are not present in the data to be scored.")

    # The function to use with sapply to determine which factors have different
    # levels in the new data versus the levels used in model estimation.
    checkLevels <- function(z) {
      current.levels <- levels(nd[[z]])
      desired.levels <- ol[[z]]
      !all(current.levels == desired.levels)
    }
    the.factors <- the.factors[sapply(the.factors, checkLevels)]
    # The function to use with lapply to relevel a factor
    relevelFac <- function(z) {
      orig.factor <- nd[[z]]
      these.levels <- ol[[z]]
      new.factor <- factor(orig.factor, levels = these.levels)
      new.factor
    }
    # Relevel the factors that differ from their levels in model estimation
    new.factor.list <- lapply(the.factors, relevelFac)
    names(new.factor.list) <- the.factors
    nd[the.factors] <- new.factor.list
  }
  nd
}

#' Get X Levels from model object
#'
#' @param x model object
#' @export
getXlevels <- function(x){
  UseMethod('getXlevels')
}

#' @export
getXlevels.default <- function(x){
  x$xlevels
}

#' @export
getXlevels.svm.formula <- function(x){
  noZeroLevels(x$xlevels)
}

#' @export
getXlevels.naiveBayes <- function(x){
  noZeroLevels(x$xlevels)
}

#' @export
getXlevels.rpart <- function(x){
  attr(x, "xlevels")
}

#' @export
getXlevels.randomForest.formula <- function(x){
  noZeroLevels(x$forest$xlevels)
}

#' @export
getXlevels.gbm <- function(x){
  xlevels <- x$var.levels[x$var.type != 0]
  names(xlevels) <- x$var.names[x$var.type != 0]
  noZeroLevels(xlevels)
}

#' @export
getXlevels.C5.0 <- function(x){
  noNullLevels(x$xlevels)
}

#' Get Y levels from model object
#'
#' @param x model object.
#' @param ... other parameters to pass to the function.
#' @export
getYlevels <- function(x, ...){
  UseMethod("getYlevels")
}

#' @export
getYlevels.default <- function(x, ...){
  x$levels
}

#' @export
getYlevels.randomForest.formula <- function(x, ...){
  levels(x$y)
}

#' @export
getYlevels.rpart <- function(x, new.data, ...){
  attributes(predict(x, newdata = new.data[1, , drop = FALSE]))$dimnames[[2]]
}

#' @export
getYlevels.glm <- function(x, ...) {
# Should this be as in getYlevels.svyglm below?
  if (family(x)$family != "binomial") {
    return(NULL)
  }
  y_name <- as.character(formula(x))[2]
  y_var <- eval(parse(text = paste0("x$data$", y_name)))
  levels(y_var)
}

#' @export
getYlevels.svyglm <- function(x, ...) {
  # The assumption here is we only return Y levels for classification
  # models, but svyglm can be used for regression models too?
  if (!family(x)$family %in% c("quasibinomial", "binomial")){
    return(NULL)
  }
#  original code:
#  y_name <- unlist(strsplit(as.character(x$call)[2], " ~ "))[1]
#  code consistent with getYlevels.glm
  y_name <- as.character(formula(x))[2]
  y_var <- eval(parse(text = paste0("x$data$", y_name)))
  levels(y_var)
}

#' @export
getYlevels.gbm <- function(x, ...) {
  if(is.null(x$classes) && !is.null(x$target.levels)) {
    x$target.levels
  } else {
    x$classes
  }
}

#' @export
getYlevels.nnet.formula <- function(x, ...) {
  x$lev
}

#' @export
getYlevels.earth <- function(x, ...) {
  x$y.levels
}

#' @export
getYlevels.svm <- function(x, ...){
  if (x$type <= 2){
    x$levels[x$labels]
  } else {
    NULL
  }
}

#' @export
getYlevels.C5.0 <- function(x, ...){
  x$ylevels
}

#' @export
getYlevels.glmnet <- function(x, ...){
  x$ylevels
}

#' @export
getYlevels.cv.glmnet <- function(x, ...){
  x$ylevels
}

## Predict probabilities ----
predProb <- function(x, new.data, ...){
  UseMethod('predProb')
}

predProb.rpart <- function(x, new.data, ...){
  predict(x, newdata = new.data)
}

predProb.naiveBayes <- function(x, new.data, type){
  predict(x, newdata = new.data, type="raw")
}

predProb.svm <- function(x, new.data){
  pred = predict(x, newdata = new.data, decision.values = TRUE, probability = TRUE,
                 na.action = na.omit)
  attr(pred, "probabilities")
}

predProb.randomForest <- function(x, new.data){
  predict(x, newdata = new.data, type = "prob")
}

# The predict methods of the gbm package are a moving target, below handles
# the cases we have seen so far.
predProb.gbm <- function(x, new.data) {
  if (!(x$distribution %in% c("bernoulli", "multinomial", "adaboost"))) {
    return(NULL)
  }
  pred <- predict(x, newdata = new.data, type = "response", n.trees = x$best.trees)
  if (class(pred) == "array") {
    pred1 <- pred[,,1]
    if (inherits(pred1, 'numeric')) pred1 <- t(as.matrix(pred1))
  }
  as.data.frame(pred1)
}

predProb.glm <- function(x, new.data) {
  if (family(x)$family != "binomial") {
    return(NULL)
  }
  pred <- predict(x, newdata = new.data, type = "response")
  cbind(1 - pred, pred)
}

predProb.svyglm <- function(x, new.data) {
  if (family(x)$family != "quasibinomial") {
    return(NULL)
  }
  pred <- predict(x, newdata = new.data, type = "response")
  cbind(1 - pred, pred)
}

predProb.nnet.formula <- function(x, new.data) {
  pred <- predict(x, newdata = new.data, type = "raw")
  if(ncol(pred) == 1) {
    cbind(1 - pred, pred)
  } else {
    pred
  }
}

predProb.earth <- function(x, new.data) {
  if (x$glm.list[[1]]$family$family != "binomial") {
    return(NULL)
  }
  pred <- predict(x, newdata = new.data, type = "response")
  if (ncol(pred) == 1) {
    cbind(1 - pred, pred)
  } else {
    pred
  }
}

## The estimation sample percentage of the target level of interest ----
samplePct <- function(x, new.data, ...){
  UseMethod('samplePct')
}

samplePct.default <- function(x, os.value, new.data) {
  y.levels <- getYlevels(x)
  100*sum(as.numeric(y.levels == os.value)) / length(y.levels)
}

samplePct.glm <- function(x, target.value, new.data) {
  y.name <- unlist(strsplit(as.character(x$call)[2], " ~ "))[1]
  y.var <- eval(parse(text = paste("x$data$", y.name, sep = "")))
  100*sum(as.numeric(as.character(y.var) == target.value)) / length(y.var)
}

samplePct.gbm <- function(x, os.value, new.data) {
  x$sample.pct[x$target.levels == os.value]
}

samplePct.earth <- function(x, os.value, new.data) {
  y.var <- x$glm.list[[1]]$y
  y.levels <- getYlevels(x)
  sample.pct <- 100*sum(y.var) / length(y.var)
  if((1:2)[y.levels == os.value] == 1) {
    sample.pct <- 100 - sample.pct
  }
  sample.pct
}

samplePct.rpart <- function(x, os.value, new.data) {
  AlteryxRDataX::AlteryxMessage("In predProb.rpart", iType = 2, iPriority = 3)
  y.levels <- getYlevels(x, new.data)
  if (y.levels[1] == os.value) {
    new.y <- 2 - x$y
  } else {
    new.y <- x$y - 1
  }
  100*sum(new.y) / length(new.y)
}

samplePct.nnet.formula <- function(x, os.value, new.data) {
  100*x$target.counts[names(x$target.counts) == os.value] / sum(x$target.counts)
}

handleNA <- function(x, ...){
  UseMethod('handleNA')
}

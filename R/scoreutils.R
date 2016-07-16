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
# Extract predictor variables from an R model call object
getXVars <- function(x){
  UseMethod('getXVars')
}

getXVars.default <- function(x) {
  the.call <- x$call
  if(class(the.call) != "call") stop("The argument was not a call object.")
  the.formula <- as.character(the.call)[2]
  form.split <- unlist(strsplit(the.formula, " ~ "))
  xvars <- unlist(strsplit(form.split[2], " \\+ "))
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

getXVars.naiveBayes <- function(x) {
  x$xvars
}

getXVars.svm.formula <- getXVars.naiveBayes

noZeroLevels <- function(ll){Filter(Negate(is.numeric), ll)}
noNullLevels <- function(ll){Filter(Negate(is.null), ll)}

# matchLevels coerces the levels in new data factors to exactly match the levels
# of factors in the original data, and is needed for Revo ScaleR models
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
      stop("There are factor variables in the model that are not present in the data to be scored.")

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

## Get X Levels ----
getXlevels <- function(x){
  UseMethod('getXlevels')
}

getXlevels.default <- function(x){
  x$xlevels
}

getXlevels.svm.formula <- function(x){
  noZeroLevels(x$xlevels)
}

getXlevels.naiveBayes <- function(x){
  noZeroLevels(x$xlevels)
}

getXlevels.rpart <- function(x){
  attr(x, "xlevels")
}

getXlevels.randomForest.formula <- function(x){
  noZeroLevels(x$forest$xlevels)
}

getXlevels.gbm <- function(x){
  xlevels <- x$var.levels[x$var.type != 0]
  names(xlevels) <- x$var.names[x$var.type != 0]
  noZeroLevels(xlevels)
}


## Get Y levels ----
getYlevels <- function(x, ...){
  UseMethod("getYlevels")
}

getYlevels.default <- function(x, ...){
  x$levels
}

getYlevels.randomForest.formula <- function(x, ...){
  levels(x$y)
}

getYlevels.rpart <- function(x, new.data){
  attributes(predict(x, newdata = new.data[1, , drop = FALSE]))$dimnames[[2]]
}

getYlevels.glm <- function(x, ...) {
  if (family(x)$family != "binomial") {
    return(NULL)
  }
  y_name <- unlist(strsplit(as.character(x$call)[2], " ~ "))[1]
  y_var <- eval(parse(text = paste0("x$data$", y_name)))
  levels(y_var)
}

getYlevels.svyglm <- function(x, ...) {
  if (family(x)$family != "quasibinomial") {
    return(NULL)
  }
  y_name <- unlist(strsplit(as.character(x$call)[2], " ~ "))[1]
  y_var <- eval(parse(text = paste0("x$data$", y_name)))
  levels(y_var)
}

getYlevels.gbm <- function(x, ...) {
  if(is.null(x$classes) && !is.null(x$target.levels)) {
    x$target.levels
  } else {
    x$classes
  }
}

getYlevels.nnet.formula <- function(x, ...) {
  x$lev
}

getYlevels.earth <- function(x, ...) {
  x$y.levels
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
    pred <- as.matrix(pred[,,1])
  }
  if (is.matrix(pred) || is.data.frame(pred)) {
    if (ncol(pred) == 1) {
      out_pred <- cbind(1 - pred, pred)
    } else {
      out_pred <- pred
    }
  } else {
    out_pred <- cbind(1 - pred, pred)
  }
  out_pred
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
  y.name <- unlist(strsplit(as.character(mod.obj$call)[2], " ~ "))[1]
  y.var <- eval(parse(text = paste("mod.obj$data$", y.name, sep = "")))
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
  AlteryxMessage("In predProb.rpart", iType = 2, iPriority = 3)
  y.levels <- getYlevels(x, new.data)
  if (y.levels[1] == os.value) {
    new.y <- 2 - mod.obj$y
  } else {
    new.y <- mod.obj$y - 1
  }
  100*sum(new.y) / length(new.y)
}

samplePct.nnet.formula <- function(x, os.value, new.data) {
  100*x$target.counts[names(x$target.counts) == os.value] / sum(x$target.counts)
}

handleNA <- function(x, ...){
  UseMethod('handleNA')
}

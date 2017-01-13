genearateDataForPlotLogReg <- function(d, extras, config) {
  thresholds <- seq(0, 1, .05)
  plyr::ldply(thresholds,
              computeBinaryMetrics,
              actual = ifelse(d$actual == extras$posClass, TRUE, FALSE),
              pred_prob = d[[paste0('Score_', extras$posClass)]]
  )
}

#' get results from running cross validation for logistic regression model
#'
#' @param inputs inputs list with data and models
#' @param config list of configuration options
#' @return list of results for outputting
#' @export
getResultsCrossValidationLogReg <- function(inputs, config) {
  ###### WORKAROUND #########################################
  # Namespace issues require redefining the call and family
  # Time is not permitting me to make proper changes
  if (!config$regularization) {
    model <- inputs$models[[1]]
    model$call$formula <- makeFormula(config$`X Vars`, config$`Y Var`)
    model$call$family <- binomial(config$Link)
    inputs$models[[1]] <- model
  } else {
    model <- inputs$models[[1]]
    model$yvar <- config$`Y Var`
    inputs$models[[1]] <- model
  }
  ### END WORKAROUND

  inputs$data$recordID <- 1:NROW(inputs$data)
  yVarList <- getYvars(inputs$data, inputs$models)
  y_name <- yVarList$y_name
  yVar <- yVarList$y_col
  inputs$modelNames <- modelNames <- names(inputs$models)

  checkXVars(inputs)

  if ((config$classification) && (length(unique(yVar)) == 2)) {
    if ((is.null(config$posClass)) || (config$posClass == "")) {
      config$posClass <- as.character(getPositiveClass(levels(yVar)))
    }
  }

  extras <- list(
    yVar = yVar,
    y_name = y_name,
    posClass = config$posClass,
    allFolds = createFolds(data = inputs$data, config = config, seed = config$seed),
    levels = levels(yVar)
  )

  dataOutput1 <- generateOutput1(inputs, config, extras)
  preppedOutput1 <- data.frame(
    RecordID = dataOutput1$recordID,
    Trial = dataOutput1$trial,
    Fold = dataOutput1$fold,
    Model = modelNames[dataOutput1$mid],
    Response = dataOutput1$response,
    Actual = dataOutput1$actual
  )
  dataOutput2 <- generateOutput2(dataOutput1, extras, modelNames)
  preppedOutput2 <- reshape2::melt(dataOutput2, id = c('trial', 'fold', 'Model'))

  confMats <- generateOutput3(dataOutput1, extras, modelNames)

  plotData <- plyr::ddply(
    dataOutput1,
    c("trial", "fold", "mid"),
    genearateDataForPlotLogReg,
    extras = extras,
    config = config
  )

  outputPlot <- plotBinaryData(plotData, config, modelNames)

  return(list(
    data = preppedOutput1,
    fitMeasures = preppedOutput2,
    confMats = confMats,
    outputPlot = outputPlot
    )
  )
}

#' Run CV and output results for logistic CV model
#'
#' @param inputs inputs list with data and models
#' @param config list of configuration options
#' @return list of results for outputting
#' @export
runCrossValidationLogReg <- function(inputs, config) {
  n <- NROW(inputs$data)
  p <- NCOL(inputs$data) - 1
  trials <- config$numberTrials
  results <- getResultsCrossValidationLogReg(inputs, config)
  write.Alteryx2(results$fitMeasures, 3)
  AlteryxGraph2(results$outputPlot, 4)
  # results is a list including data frames fitMeasures and confMats
  fitness_metrics <- plyr::daply(
    .data = results$fitMeasures,
    .variables = c('variable'),
    .fun = function(df){
      # na.rm also filters NaNs, which lets us return a meaningful
      # value more often
      mean(x = df$value, na.rm = TRUE)
    }
  )
  conf_mats_df <- results$confMats
  conf_mats_df <- conf_mats_df[, c('Predicted_class', 'Variable', 'Value')]
  names(conf_mats_df) <- c('predicted', 'actual', 'count')
  conf_mats_df$actual <- gsub('Class_', '', conf_mats_df$actual)
  conf_mats_df$count <- as.numeric(conf_mats_df$count)
  # We need to construct a 2x2 confusion matrix and update it, to guard
  # against the case where all records are predicted to belong to a single
  # class, in which case other aggregation methods (e.g. plyr::daply, see
  # below) might not produce a full-sized confusion matrix.
  actual_value_v <- unique(conf_mats_df[, 2])
  confusion_matrix_df <- data.frame(
    c(0, 0),
    c(0, 0)
  )
  names(confusion_matrix_df) <- actual_value_v
  rownames(confusion_matrix_df) <- actual_value_v
  for(i in 1:nrow(conf_mats_df)){
    predicted <- conf_mats_df$predicted[i]
    actual <- conf_mats_df$actual[i]
    confusion_matrix_df[predicted, actual] <-
      confusion_matrix_df[predicted, actual] +
      conf_mats_df$count[i]
  }
  confusion_matrix_df <- confusion_matrix_df / trials
  # The above is also equivalent to the below, but the below only produces
  # a single row in the pathological case.
  #foo_df <- plyr::daply(
  #  .data = conf_mats_df,
  #  .variables = c('predicted', 'actual'),
  #  .fun = function(df){sum(df$count)}
  #)
  #foo_df <- foo_df / trials
  return_value_v <- rep_len(
    x = 0,
    length.out = 8
  )
  names(return_value_v) <- c(
    'accuracy',
    'precision',
    'recall',
    'f1',
    'pred_pos_actual_pos',
    'pred_pos_actual_neg',
    'pred_neg_actual_pos',
    'pred_neg_actual_neg'
  )
  positive_class_index <- which(actual_value_v == getPositiveClass(target_levels = actual_value_v))
  negative_class_index <- which(actual_value_v != getPositiveClass(target_levels = actual_value_v))

  return_value_v['accuracy'] <- fitness_metrics['Accuracy_Overall']
  return_value_v['precision'] <- confusion_matrix_df[2, 2] / sum(confusion_matrix_df[2, ])
  return_value_v['recall'] <- confusion_matrix_df[2, 2] / sum(confusion_matrix_df[, 2])
  return_value_v['f1'] <- fitness_metrics['F1']
  return_value_v['pred_pos_actual_pos'] <- confusion_matrix_df[positive_class_index, positive_class_index]
  return_value_v['pred_pos_actual_neg'] <- confusion_matrix_df[positive_class_index, negative_class_index]
  return_value_v['pred_neg_actual_pos'] <- confusion_matrix_df[negative_class_index, positive_class_index]
  return_value_v['pred_neg_actual_neg'] <- confusion_matrix_df[negative_class_index, negative_class_index]
  return(return_value_v)
}

getResultsCrossValidationLinReg <- function(inputs, config){
  n <- NROW(inputs$data)
  p <- NCOL(inputs$data) - 1
  inputs$data$recordID <- 1:n
  yVarList <- getYvars(inputs$data, inputs$models)
  yVar <- yVarList$y_col
  y_name <- yVarList$y_name
  inputs$modelNames <- names(inputs$models)
  modelNames <- names(inputs$models)
  checkXVars(inputs)
  extras <- list(
    yVar = yVar,
    y_name = y_name,
    posClass = config$posClass,
    allFolds = createFolds(data = inputs$data, config = config, seed = config$seed),
    levels = NULL
  )
  dataOutput1 <- generateOutput1(inputs, config, extras)
  if ("Score" %in% colnames(dataOutput1)) {
    dataOutput1 <- data.frame(trial = dataOutput1$trial, fold = dataOutput1$fold,
                              mid = dataOutput1$mid, recordID = dataOutput1$recordID,
                              response = dataOutput1$Score, actual = dataOutput1$actual)
  }
  preppedOutput1 <- data.frame(RecordID = dataOutput1$recordID,
                               Trial = dataOutput1$trial, Fold = dataOutput1$fold,
                               Model = modelNames[dataOutput1$mid], Response = dataOutput1$response,
                               Actual = dataOutput1$actual)

  dataOutput2 <- generateOutput2(dataOutput1, extras, modelNames)
  preppedOutput2 <- reshape2::melt(dataOutput2, id = c('trial', 'fold', 'Model'))
  plotData <- ddply(dataOutput1, .(trial, fold, mid), generateDataForPlotsLinReg,
                    extras = extras, config = config
  )
  outputPlot <- plotRegressionData(plotData, config, modelNames)
  list(
    data = preppedOutput1, fitMeasures = preppedOutput2,
    outputPlot = outputPlot
  )
}

#' process for Linear Regression with Cross Validation
#' returns named vector of six model-performance metrics
#' r_squared, adj_r_squared, avg_mae, avg_mape, avg_mse, avg_rmse
#'
#' @param inputs list of inputs
#' @param config list of config elements
#' @export
runCrossValidationLinReg <- function(inputs, config){
  n <- NROW(inputs$data)
  p <- NCOL(inputs$data) - 1
  results <- getResultsCrossValidationLinReg(inputs, config)
  # write.Alteryx2(results$data, 2)
  write.Alteryx2(results$fitMeasures, 3)
  AlteryxGraph2(results$outputPlot, 4)
  # results are a list.
  # results$fitMeasures is a df
  fitness_metrics <- plyr::daply(
    .data = results$fitMeasures,
    .variables = c('variable'),
    .fun = function(df){mean(df$value)}
  )
  return_value_v <- rep_len(x = 0, length.out = 6)
  names(return_value_v) <- c(
    'r_squared',
    'adj_r_squared',
    'avg_mae',
    'avg_mape',
    'avg_mse',
    'avg_rmse'
  )
  return_value_v['r_squared'] <- fitness_metrics['Correlation']^2
  return_value_v['adj_r_squared'] <- adj_r_squared(
    r_squared = fitness_metrics['Correlation']^2,
    n = n,
    p = p,
    intercept_degrees_freedom = as.numeric(!config$`Omit Constant`)
  )
  return_value_v['avg_mae'] <- fitness_metrics['MAE']
  return_value_v['avg_mape'] <- fitness_metrics['MAPE']
  return_value_v['avg_mse'] <- fitness_metrics['RMSE']^2
  return_value_v['avg_rmse'] <- fitness_metrics['RMSE']
  return(return_value_v)
}

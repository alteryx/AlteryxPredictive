getResultsCrossValidationLinReg <- function(inputs, config){
  inputs$data$recordID <- 1:NROW(inputs$data)
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
#'
#' @param inputs list of inputs
#' @param config list of config elements
#' @export
runCrossValidationLinReg <- function(inputs, config){
  results <- getResultsCrossValidationLinReg(inputs, config)
  # write.Alteryx2(results$data, 2)
  write.Alteryx2(results$fitMeasures, 3)
  AlteryxGraph2(results$outputPlot, 4)
}

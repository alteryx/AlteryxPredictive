#' @import ggplot2
#' @import reshape2
getResultsCrossValidation <- function(inputs, config){
  inputs$data$recordID <- 1:NROW(inputs$data)
  yVarName <- getYVar(inputs$models$Decision_tree)
  yVar <- inputs$data[[yVarName]]
  if ((config$classification) && (length(unique(yVar)) == 2)) {
    if ((is.null(config$posClass)) || (config$posClass == "")) {
      config$posClass <- as.character(getPosClass(config, levels(yVar)))
    }
  }

  inputs$modelNames <- names(inputs$models)
  modelNames <- names(inputs$models)

  extras <- list(
    yVar = yVar,
    posClass = config$posClass,
    allFolds = createFolds(data = inputs$data, config = config),
    levels = if (config$classification) levels(yVar) else NULL
  )

  dataOutput1 <- generateOutput1(inputs, config, extras)
  if ((config$regression) && ("Score" %in% colnames(dataOutput1))) {
    dataOutput1 <- data.frame(trial = dataOutput1$trial, fold = dataOutput1$fold,
                              mid = dataOutput1$mid, recordID = dataOutput1$recordID,
                              response = dataOutput1$Score, actual = dataOutput1$actual)
  }

  preppedOutput1 <- if (config$regression) {
    data.frame(RecordID = dataOutput1$recordID,
               Trial = dataOutput1$trial, Fold = dataOutput1$fold,
               Model = modelNames[dataOutput1$mid], Response = dataOutput1$response,
               Actual = dataOutput1$actual
    )
  } else {
    data.frame(RecordID = dataOutput1$recordID,
               Trial = dataOutput1$trial, Fold = dataOutput1$fold,
               Model = modelNames[dataOutput1$mid], Response = dataOutput1$response,
               Actual = dataOutput1$actual
    )
  }
  #write.Alteryx2(preppedOutput1, nOutput = 1)
  dataOutput2 <- generateOutput2(dataOutput1, extras, modelNames)
  preppedOutput2 <- reshape2::melt(dataOutput2, id = c('trial', 'fold', 'Model'))
  #write.Alteryx2(preppedOutput2, nOutput = 2)

  confMats <- if (config$classification) {
    generateOutput3(dataOutput1, extras, modelNames)
    #write.Alteryx2(confMats, 3)
  } else {
    #Provide garbage data that'll get filtered out on the Alteryx side.
    data.frame(Trial = 1, Fold = 1, Model = 'model', Type = 'Regression',
               Predicted_class = 'no', Variable = "Classno", Value = 50
    )
  }
  plotData <- plyr::ddply(dataOutput1, c("trial", "fold", "mid"), generateDataForPlots,
                    extras = extras, config = config
  )
  outputPlot <- if (config$classification) {
    if (length(extras$levels) == 2) {
      plotBinaryData(plotData, config, modelNames)
    } else {
      # Generate an empty plot
      empty_df <- data.frame()
      emptyPlot <- ggplot2::ggplot(empty_df) + ggplot2::geom_point() + ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
        ggplot2::ggtitle("No plots available for >2 class classification")
    }
  } else {
    plotRegressionData(plotData, config, modelNames)
  }
  list(
    data = preppedOutput1, fitMeasures = preppedOutput2,
    confMats = confMats, outputPlot = outputPlot
  )
}


#' Wrapper function for performing cross-validation on DTree models.
#'
#' @param config list of config options
#' @param inputs list containing a Dtree model and the data used to generate it
#' @return list with components measuring model fit obtained via CV
runCrossValidationDTree <- function(inputs, config){
  #Ensure that config$regression and config$classification are both set properly.
  #Note that with an || in R, the LHS is evaluated first. The RSH is only evaluated
  #if the LHS is false. We're relying on that property of R here. Only rpart objects
  #have a method component, so we only want to look at the RHS if we're in the rpart
  #(ie not C5.0) case.
  mod_obj <- inputs$models$Decision_tree
  if (((config$`model.algorithm`) == 'C5.0') || ((mod_obj$method) == "class")) {
    config$classification <- TRUE
    config$regression <- FALSE
  } else {
    config$classification <- FALSE
    config$regression <- TRUE
  }
  inputs$data <- inputs$the.data
  cv_results <- getResultsCrossValidation(inputs, config)
#   write.Alteryx2(results$data, 1)
#   write.Alteryx2(results$fitMeasures, 2)
#   write.Alteryx2(results$confMats, 3)
#   AlteryxGraph2(results$outputPlot, 4)
  return(cv_results)
}

#' @import ggplot2
#' @import reshape2
getResultsCrossValidationDTree <- function(inputs, config){
  config$posClass <- verifyClass(inputs$data[,1], config$posClass)
  inputs$data$recordID <- 1:NROW(inputs$data)
  yVarName <- getYVar(inputs$models$Decision_Tree)
  yVar <- inputs$data[[yVarName]]
  if ((config$classification) && (length(unique(yVar)) == 2)) {
    if ((is.null(config$posClass)) || (config$posClass == "")) {
      config$posClass <- as.character(getPositiveClass(levels(yVar)))
    }
  }

  inputs$modelNames <- names(inputs$models)
  modelNames <- names(inputs$models)
  extras <- list(
    yVar = yVar,
    posClass = config$posClass,
    allFolds = createFolds(
      data = inputs$data,
      config = config,
      set_seed = config$set_seed_cv,
      seed = config$cv_seed
      ),
    levels = if (config$classification) levels(yVar) else NULL
  )

  dataOutput1 <- generateOutput1(inputs, config, extras)
  if ((config$regression) && ("Score" %in% colnames(dataOutput1))) {
    dataOutput1 <- data.frame(trial = dataOutput1$trial, fold = dataOutput1$fold,
                              mid = dataOutput1$mid, recordID = dataOutput1$recordID,
                              response = dataOutput1$Score, actual = dataOutput1$actual)
  }


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
  plotData <- plyr::ddply(dataOutput1, c("trial", "fold", "mid"), generateDataForPlotsDTree,
                    extras = extras, config = config
  )
  if (config$`display.static`) {
    if (config$classification) {
      if (length(extras$levels) == 2) {
        plotBinaryData(plotData, config, modelNames)
      } else {
        # Generate an empty plot
        empty_df <- data.frame()
        emptyPlot <- ggplot2::ggplot(empty_df) + ggplot2::geom_point() + ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
          ggplot2::ggtitle("No plots available for >2 class classification")
        AlteryxGraph2(emptyPlot, nOutput = 4)
      }
    } else {
      plotRegressionData(plotData, config, modelNames)
    }
  } else {
    empty_df <- data.frame()
    emptyPlot <- ggplot2::ggplot(empty_df) + ggplot2::geom_point() + ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
    AlteryxGraph2(emptyPlot, nOutput = 4)
  }
  list(
    fitMeasures = preppedOutput2,
    confMats = confMats
  )
}


#' Wrapper function for performing cross-validation on DTree models.
#'
#' @param config list of config options
#' @param inputs list containing a Dtree model and the data used to generate it
#' @return list with components measuring model fit obtained via CV
runCrossValidationDTree <- function(inputs, config){
  #Ensure that config$regression and config$classification are both set properly.
  #Note that with an || in R, the LHS is evaluated first. The RHS is only evaluated
  #if the LHS is false. We're relying on that property of R here. Only rpart objects
  #have a method component, so we only want to look at the RHS if we're in the rpart
  #(ie not C5.0) case.
  if(config$use.weights && config$model.algorithm == "C5.0") {
    config$use.weights <- FALSE
  }
  mod_obj <- inputs$models$Decision_Tree
  if (((config$`model.algorithm`) == 'C5.0') || ((mod_obj$method) == "class")) {
    config$classification <- TRUE
    config$regression <- FALSE
  } else {
    config$classification <- FALSE
    config$regression <- TRUE
  }
  inputs$data <- inputs$the.data
  cv_results <- getResultsCrossValidationDTree(inputs, config)
  if (config$`display.static`) {
    write.Alteryx2(cv_results$fitMeasures, 1)
    maxLevels <- 4
    if(length(unique(cv_results$confMats$Variable)) > maxLevels) {
      ### Trick alteryx with data that will be thrown away
      cv_results$confMats$Type <- "Throw-away"
    }
    write.Alteryx2(cv_results$confMats, 2)
  }
   #AlteryxGraph2(cv_results$outputPlot, 3)
   #Graphing is done in plotBinaryData/plotRegressionData
  return(cv_results)
}

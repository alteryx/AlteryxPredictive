writeOutputs <- function(results, ...) {
  UseMethod('writeOutputs')
}

writeOutputs.GLM <- function(results, config){
  # Report
  write.Alteryx2(results$report, nOutput = 1)

  # Plot Output
  # whr <- graphWHR(inches = "True", in.w = 6, in.h = 6, config$resolution)
  whr <- graphWHR2(inches = TRUE, in.w = 6, in.h = 6,
    graph.resolution = config$graph.resolution)
  AlteryxGraph2(results$plot(), 2, width = whr[1], height = whr[2],
    res = whr[3], pointsize = 9)

  # Write out empty plots so MetaInfo is filled out.
  AlteryxGraph2(emptyPlot(), nOutput = 4)
  AlteryxGraph2(emptyPlot(), nOutput = 5)

  # Model Object
  the.obj <- prepModelForOutput(config$`Model Name`, results$model)
  write.Alteryx2(the.obj, nOutput = 3)
}


writeOutputs.GLMNET <- function(results, config) {
  write.Alteryx2(results$coefficients, nOutput = 1)
  results$model$coefficients <- (results$coefficients)[,2]
  names(results$model$coefficients) <- (results$coefficients)[,1]
  if (config$display_graphs) {
    list_obj_to_plot <- c('norm', 'lambda', 'dev')
    plot_obj <- results$model
    if (config$internal_cv) {
      AlteryxGraph2(plot(results$model), nOutput = 5)
      plot_obj <- plot_obj$glmnet.fit
    }
    #Note: We're using different outputs for these because there currently
    #appears to be a bug. An error frequently occurs when they're all sent
    #to the same output.
    AlteryxGraph2(plot(plot_obj, xvar = list_obj_to_plot[1]), nOutput = 2)
    AlteryxGraph2(plot(plot_obj, xvar = list_obj_to_plot[2]), nOutput = 2)
    AlteryxGraph2(plot(plot_obj, xvar = list_obj_to_plot[3]), nOutput = 4)
  }
  the.obj <- prepModelForOutput(config$`Model Name`, results$model)
  write.Alteryx2(the.obj, nOutput = 3)
}

writeOutputs.DecisionTree <- function(results, config) {
  # Report Output
  write.Alteryx2(results$report, nOutput = 1)
  if(class(results$model) == "C5.0" && config$rules && config$tree.plot) {
    AlteryxMessage2("Tree Plot not available for C5.0 rules-based model", 2, 2)
    config$tree.plot <- FALSE
  }
  # Tree Plot
  whr <- graphWHR2(inches = config$tree.inches, in.w = config$tree.in.w,
    in.h = config$tree.in.h, cm.w = config$tree.cm.w, cm.h = config$tree.cm.h,
    graph.resolution = config$tree.graph.resolution, print.high = TRUE
  )
  if (config$tree.plot) {
    AlteryxGraph2(results$treePlot(), nOutput = 2, width = whr[1], height = whr[2],
                  res = whr[3], pointsize = config$tree.pointsize
    )
  } else {
    #Write out garbage data that will get filtered out on the Alteryx side
    AlteryxGraph2(emptyPlot(), nOutput = 2)
  }

  # Model Object
  the.obj <- prepModelForOutput(config$`Model Name`, results$model)
  write.Alteryx2(the.obj, nOutput = 3)

  # Prune Plot
  whr <- graphWHR2(inches = config$prune.inches, in.w = config$prune.in.w,
    in.h = config$prune.in.h, cm.w = config$prune.cm.w, cm.h = config$prune.cm.h,
    graph.resolution = config$prune.graph.resolution, print.high = FALSE
  )
  if (config$prune.plot) {
    AlteryxGraph2(results$prunePlot(), nOutput = 4, width = whr[1], height = whr[2],
                  res = whr[3], pointsize = config$prune.pointsize
    )
  } else {
    #Write out garbage data that will get filtered out on the Alteryx side
    AlteryxGraph2(emptyPlot(), nOutput = 4)
  }
  # Interactive Dashboard
  flightdeck::fdRender(x = results$dashboard, nOutput = 5)
}

# Logistic Regression ----
getResultsLogisticRegression <- function(inputs, config){
  config$`Model Name`= validName(config$`Model Name`)
  if ((is.null(config$regularization))||(!(config$regularization))) {

    requireNamespace("car")
    # Modify the link so that it can be passed on to R.
    if (config$Link == "complementary log-log"){
      config$Link <- "cloglog"
    }

    if (inputs$XDFInfo$is_XDF){
      d <- processLogisticXDF(inputs, config)
      glm.out <- createReportLogisticXDF(d$the.model, config, d$null.model)
      plot.out <- function(){createPlotOutputsLogisticXDF()}
    } else {
      d <- processLogisticOSR(inputs, config)
      glm.out <- createReportLogisticOSR(d$the.model, config, d$model_type)
      plot.out <- function(){
        createPlotOutputsLogisticOSR(d$the.model, FALSE, config)
      }
    }
    d$the.model$config <- config
    results <- list(model = d$the.model, report = glm.out, plot = plot.out)
    class(results) <- "GLM"
  } else {
    config$posClass <- verifyClass(inputs$the.data[,1], config$posClass)
    the.model <- processElasticNet(inputs, config)
    the.model$config <- config
    #We don't need to worry about backwards compatibility in this section.
    #In order to enter this side of the outer if loop, config$regularization
    #must exist and be true. Thus, config$display_graphs must exist as well.
    results <- list(model = the.model)
    coefs_out <- createReportGLMNET(the.model)
    results <- append(results, list(coefficients = coefs_out))
    class(results) <- "GLMNET"
  }
  results
}

runLogisticRegression <- function(inputs, config){
  # reverse compatability code start
  if (!("regularization" %in% names(config)))
    config$regularization <- FALSE
  # reverse compatability code end

  if (config$regularization) {
    inputs$the.data <- checkMissing.omit(inputs$the.data)
    if ((config$internal_cv) && (config$nfolds > NROW(inputs$the.data))) {
      AlteryxMessage2("You chose more folds for internal cross-validation than the number of valid rows in your data.", iType = 2, iPriority = 3)
      AlteryxMessage2("The number of folds used is being re-set to the number of valid rows in your data.", iType = 2, iPriority = 3)
      config$nfolds <- NROW(inputs$the.data)
    }
  }
  results <- getResultsLogisticRegression(inputs, config)
  writeOutputs(results, config)
}

# Linear Regression ----
getResultsLinearRegression <- function(inputs, config){
  requireNamespace("car")
  config$`Model Name`= validName(config$`Model Name`)
  if ((is.null(config$regularization))||(!(config$regularization))) {
    if (inputs$XDFInfo$is_XDF){
      the.model <- processLinearXDF(inputs, config)
      lm.out <- createReportLinearXDF(the.model, config)
      plot.out <- function(){createPlotOutputsLinearXDF()}
    } else {
      the.model <- processLinearOSR(inputs, config)
      lm.out <- createReportLinearOSR(the.model, config)
      plot.out <- function(){createPlotOutputsLinearOSR(the.model)}
    }
    the.model$config <- config
    results <- list(model = the.model, report = lm.out, plot = plot.out)
    class(results) <- "GLM"
  } else {
    the.model <- processElasticNet(inputs, config)
    the.model$config <- config
    #We don't need to worry about backwards compatibility in this section.
    #In order to enter this side of the outer if loop, config$regularization
    #must exist and be true. Thus, config$display_graphs must exist as well.
    results <- list(model = the.model)
    coefs_out <- createReportGLMNET(the.model)
    results <- append(results, list(coefficients = coefs_out))
    class(results) <- "GLMNET"
  }
  results
}

runLinearRegression <- function(inputs, config){
  if (config$regularization) {
    inputs$the.data <- checkMissing.omit(inputs$the.data)
    if ((config$internal_cv) && (config$nfolds > NROW(inputs$the.data))) {
      AlteryxMessage2("You chose more folds for internal cross-validation than the number of valid rows in your data.", iType = 2, iPriority = 3)
      AlteryxMessage2("The number of folds used is being re-set to the number of valid rows in your data.", iType = 2, iPriority = 3)
      config$nfolds <- NROW(inputs$the.data)
    }
  }
  results <- getResultsLinearRegression(inputs, config)
  writeOutputs(results, config)
}


# Decision Tree ----
getResultsDecisionTree <- function(inputs, config) {
  # Set the seed to get run-over-run consistency
  set.seed(1)

  if(inputs$XDFInfo$is_XDF)
    config$model.algorithm <- "rxDTree"
  # Rename arguments to be consistent with rpart.
  config <- plyr::rename(config, c(
    use.weights = 'used.weights', `Model Name` = 'model.name',
    max.bins = 'maxNumBins', min.split = "minsplit", min.bucket = 'minbucket',
    xval.folds = 'xval', max.depth = 'maxdepth', Counts = 'do.counts',
    `Branch Dist` = 'b.dist'
  ))

  config$posClass <- verifyClass(inputs$the.data[,1], config$posClass)
  config$model.name <- validName(config$model.name)
  var_names <- getNamesFromOrdered(names(inputs$the.data), config$used.weights)

  the.model <- processDT(inputs, config)

  the.report.list <- createReportDT(the.model, config, var_names, inputs$XDFInfo$xdf_path)
  the.model <- the.report.list$model
  the.model.rpart <- if(inputs$XDFInfo$is_XDF) the.report.list$model_rpart else the.model
  the.report <- the.report.list$out


  makeTreePlot <- NULL
  makePrunePlot <- NULL

  if(config$tree.plot) {
    makeTreePlot <- function(){createTreePlotDT(the.model.rpart, config, inputs)}
  }
  if(config$prune.plot) {
    makePrunePlot <- function(){createPrunePlotDT(the.model.rpart)}
  }
  dashboard <- interactive_dt(config, inputs$the.data, the.model)

  results <- list(model = the.model, report = the.report,
                  treePlot = makeTreePlot, prunePlot = makePrunePlot,
                  dashboard = dashboard
  )

  class(results) <- "DecisionTree"
  results
}

runDecisionTree <- function(inputs, config){
  # for backwards compatability to pre-C5.0,
  #   add model.algorithm arg if not there
  if (!("model.algorithm" %in% names(config))) {
    config$model.algorithm <- "C5.0"
    if (class(config) == "OSR")
      config$model.algorithm <- "rpart"
    else
      config$model.algorithm <- "rxDTree"
    config$bands.check <- FALSE
    config$GlobalPruning <- FALSE
  }
  if(config$use.weights && config$model.algorithm == "C5.0") {
    AlteryxMessage2("Case weights not currently supported with C5.0.", 2, 2)
    config$use.weights <- FALSE
  }
  if(config$model.algorithm == "C5.0" && config$rules){
    config$tree.plot <- FALSE
    AlteryxMessage2("Tree Plot not available for C5.0 when rules-based model is chosen")
  }
  results <- getResultsDecisionTree(inputs, config)
  writeOutputs(results, config)
}

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

  # Model Object
  the.obj <- prepModelForOutput(config$`Model Name`, results$model)
  write.Alteryx2(the.obj, nOutput = 3)
}

writeOutputs.DecisionTree <- function(results, config) {
  # Report Output
  write.Alteryx2(results$report, nOutput = 1)

  # Tree Plot
  whr <- graphWHR2(inches = config$tree.inches, in.w = config$tree.in.w,
    in.h = config$tree.in.h, cm.w = config$tree.cm.w, cm.h = config$tree.cm.h,
    graph.resolution = config$tree.graph.resolution, print.high = TRUE
  )
  AlteryxGraph2(results$treePlot(), nOutput = 2, width = whr[1], height = whr[2],
    res = whr[3], pointsize = config$tree.pointsize
  )

  # Model Object
  the.obj <- prepModelForOutput(config$`Model Name`, results$model)
  write.Alteryx2(the.obj, nOutput = 3)

  # Prune Plot
  whr <- graphWHR2(inches = config$prune.inches, in.w = config$prune.in.w,
    in.h = config$prune.in.h, cm.w = config$prune.cm.w, cm.h = config$prune.cm.h,
    graph.resolution = config$prune.graph.resolution, print.high = FALSE
  )
  AlteryxGraph2(results$prunePlot(), nOutput = 4, width = whr[1], height = whr[2],
    res = whr[3], pointsize = config$prune.pointsize
  )

  # Interactive Dashboard
  AlteryxRviz::renderInComposer(results$dashboard, nOutput = 5)
}

# Logistic Regression ----
getResultsLogisticRegression <- function(inputs, config){
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
  results <- list(model = d$the.model, report = glm.out, plot = plot.out)
  class(results) <- "GLM"
  results
}

runLogisticRegression <- function(inputs, config){
  results <- getResultsLogisticRegression(inputs, config)
  writeOutputs(results, config)
}

# Linear Regression ----
getResultsLinearRegression <- function(inputs, config){
  requireNamespace("car")
  config$`Model Name`= validName(config$`Model Name`)
  if (inputs$XDFInfo$is_XDF){
    the.model <- processLinearXDF(inputs, config)
    lm.out <- createReportLinearXDF(the.model, config)
    plot.out <- function(){createPlotOutputsLinearXDF()}
  } else {
    the.model <- processLinearOSR(inputs, config)
    lm.out <- createReportLinearOSR(the.model, config)
    plot.out <- function(){createPlotOutputsLinearOSR(the.model)}
  }

  results <- list(model = the.model, report = lm.out, plot = plot.out)
  class(results) <- "GLM"
  results
}

runLinearRegression <- function(inputs, config){
  results <- getResultsLinearRegression(inputs, config)
  writeOutputs(results, config)
}


# Decision Tree ----
getResultsDecisionTree <- function(inputs, config) {
  # Set the seed to get run-over-run consistency
  set.seed(1)

  # Rename arguments to be consistent with rpart.
  config <- plyr::rename(config, c(
    use.weights = 'used.weights', `Model Name` = 'model.name',
    max.bins = 'maxNumBins', min.split = "minsplit", min.bucket = 'minbucket',
    xval.folds = 'xval', max.depth = 'maxdepth', Counts = 'do.counts',
    `Branch Dist` = 'b.dist'
  ))

  config$model.name <- validName(config$model.name)
  var_names <- getNamesFromOrdered(names(inputs$the.data), config$used.weights)

  the.model <- processDT(inputs, config)
  the.report <- createReportDT(the.model, config, var_names, inputs$XDFInfo$is_XDF)
  makeTreePlot <- function(){createTreePlotDT(the.model, config)}
  makePrunePlot <- function(){createPrunePlotDT(the.model)}
  dashboard <- createDashboardDT(the.model, inputs$XDFInfo$is_XDF)

  results <- list(model = the.model, report = the.report,
    treePlot = makeTreePlot, prunePlot = makePrunePlot,
    dashboard = dashboard
  )
  class(results) <- "DecisionTree"
  results
}

runDecisionTree <- function(inputs, config){
  results <- getResultsDecisionTree(inputs, config)
  writeOutputs(results)
}

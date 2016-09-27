runLogisticRegression <- function(inputs, config){
  library(car)
  #' Modify the link so that it can be passed on to R.
  if (config$the.link == "complementary log-log"){
    config$the.link <- "cloglog"
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

  # Report Output
  write.Alteryx2(glm.out, nOutput = 1)

  # Plot Output
  whr <- graphWHR2(inches = TRUE, in.w = 6, in.h = 6, config$graph.resolution)
  AlteryxGraph2(plot.out(), 2, width = whr[1], height = whr[2],
    res = whr[3], pointsize = 9)

  # Model Output
  the.obj <- prepModelForOutput(config$model.name, d$the.model)
  write.Alteryx2(the.obj, nOutput = 3)
  return(the.obj)
}

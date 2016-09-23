#' #### Process Inputs
#'
#' These two function take `inputs` and `config` and return the model object
#' along with other elements essential to create the reports and plots
#'
#' @export
processLinearOSR <- function(inputs, config){
  var_names <- getNamesFromOrdered(names(inputs$the.data), config$used.weights)
  the.formula <- if (config$`Omit Constant`){
    makeFormula(c("-1", var_names$x), var_names$y)
  } else {
    makeFormula(var_names$x, var_names$y)
  }
  # FIXME: Revisit what we pass to the weights argument.
  if (config$used.weight){
    lm(the.formula, inputs$the.data, weights = inputs$the.data[[var_names$w]])
  } else {
    lm(the.formula, inputs$the.data)
  }
}

#' @export
processLinearXDF <- function(inputs, config){
  temp.dir <- '%Engine.TempFilePath%'
  xdf.path = XDFInfo$xdf_path
  var_names <- getNamesFromOrdered(names(inputs$the.data), config$used.weights)
  the.formula = if (config$`Omit Constant`){
    makeFormula(c("-1", var_names$x), var_names$y)
  } else {
    makeFormula(var_names$x, var_names$y)
  }
  the.model <- rxLinMod(the.formula, XDFInfo$xdf_path, pweights = var_names$w,
    covCoef = TRUE, dropFirst = TRUE)

  # Add the level labels for factor predictors to use in model scoring, and
  # determine if the smearing estimator adjustment should be calculated for
  # scoring option value.
  the.model$xlevels <- getXdfLevels(makeFormula(var_names$x), xdf.path)
  sum.info <- rxSummary(makeFormula(var_names$y, ""), xdf.path)
  # See if it is possible that the maximum target value is consistent with the
  # use of a natural log transformation, and construct the smearing adjust if
  # it is.
  if (sum.info$sDataFrame[1,5] <= 709) {
    resids.path <- file.path(temp.dir, paste0(ceiling(100000*runif(1)), '.xdf'))
    rxPredict(the.model, data = xdf.path, outData = resids.path,
      computeResiduals = TRUE, predVarNames = "Pred", residVarNames = "Resid")
    resids.df <- rxReadXdf(file = resids.path)
    smear <- rxSummary(~ Resid, data = resids.path,
      transforms = list(Resid = exp(Resid)))
    the.model$smearing.adj <- smear$sDataFrame[1,2]
  }
  return(the.model)
}

#' Create Reports
#'
#' If the ANOVA table is requested then create it and add its results to the
#' key-value table. Its creation will be surpressed if the car package isn't
#' present, or if the input is an XDF file.
#' @export
createReportLinearOSR <- function(the.model, config){
  lm.out <- Alteryx.ReportLM(the.model)
  lm.out <- rbind(c("Model_Name", config$model.name), lm.out)
  lm.out <- rbind(lm.out, Alteryx.ReportAnova(the.model))
  lm.out
}

#' @export
createReportLinearXDF <- function(the.model, config){
  AlteryxMessage2("Creation of the Analysis of Variance table was surpressed due to the use of an XDF file", iType = 2, iPriority = 3)
  lm.out <- AlteryxReportRx(the.model)
  lm.out <- rbind(c("Model_Name", config$model.name), lm.out)
  lm.out
}


#' Create Plots
#'
#' Prepare the basic regression diagnostic plots if it is requested
#' and their isn't the combination of singularities and the use of
#' sampling weights.
#'
#' @export
createPlotOutputsLinearOSR <- function(the.model){
  par(mfrow=c(2, 2), mar=c(5, 4, 2, 2) + 0.1)
  plot(the.model)
}

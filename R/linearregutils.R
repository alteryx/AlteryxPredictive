#' Process Linear Regression Inputs
#'
#' These two functions take `inputs` and `config` and return the model object
#' along with other elements essential to create the reports and plots
#'
#' @param inputs input data streams to the tool
#' @param config configuration passed to the tool
#' @rdname processLinear
#' @export
processLinearOSR <- function(inputs, config){
  var_names <- getNamesFromOrdered(names(inputs$the.data), config$`Use Weight`)
  the.formula <- if (config$`Omit Constant`){
    makeFormula(c("-1", var_names$x), var_names$y)
  } else {
    makeFormula(var_names$x, var_names$y)
  }
  # FIXME: Revisit what we pass to the weights argument.
  if (config$`Use Weight`){
    weight_col <- var_names$w
    weights_v <- inputs$the.data[[weight_col]]
    # WORKAROUND
    # The code below ensures that weights_v gets saved to the execution environment
    # of lm.
    my_envir <- environment()
    lapply(
      X = 1:ncol(inputs$the.data),
      FUN = function(i){
        assign(
          x = names(inputs$the.data)[i],
          value = inputs$the.data[,i],
          envir = my_envir
        )
      }
    )

    lm(formula = the.formula, data = environment(), weights = weights_v)
  } else {
    lm(the.formula, inputs$the.data)
  }
}

#' @inheritParams processLinearOSR
#' @rdname processLinear
#' @export
processLinearXDF <- function(inputs, config){
  temp.dir <- textInput('%Engine.TempFilePath%', tempdir())
  xdf.path = inputs$XDFInfo$xdf_path
  var_names <- getNamesFromOrdered(names(inputs$the.data), config$`Use Weight`)
  the.formula = if (config$`Omit Constant`){
    makeFormula(c("-1", var_names$x), var_names$y)
  } else {
    makeFormula(var_names$x, var_names$y)
  }
  the.model <- RevoScaleR::rxLinMod(the.formula, xdf.path, pweights = var_names$w,
    covCoef = TRUE, dropFirst = TRUE)

  # Add the level labels for factor predictors to use in model scoring, and
  # determine if the smearing estimator adjustment should be calculated for
  # scoring option value.
  the.model$xlevels <- getXdfLevels(makeFormula(var_names$x, ""), xdf.path)
  sum.info <- RevoScaleR::rxSummary(makeFormula(var_names$y, ""), xdf.path)
  # See if it is possible that the maximum target value is consistent with the
  # use of a natural log transformation, and construct the smearing adjust if
  # it is.
  if (sum.info$sDataFrame[1,5] <= 709) {
    resids.path <- file.path(temp.dir, paste0(ceiling(100000*runif(1)), '.xdf'))
    RevoScaleR::rxPredict(the.model, data = xdf.path, outData = resids.path,
      computeResiduals = TRUE, predVarNames = "Pred", residVarNames = "Resid")
    resids.df <- RevoScaleR::rxReadXdf(file = resids.path)
    smear <- RevoScaleR::rxSummary(~ Resid, data = resids.path,
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
#'
#' @param the.model model object
#' @param config configuration passed to the tool
#' @export
#' @rdname createReportLinear
createReportLinearOSR <- function(the.model, config){
  lm.out <- Alteryx.ReportLM(the.model)
  lm.out <- rbind(c("Model_Name", config$`Model Name`), lm.out)
  lm.out <- rbind(lm.out, Alteryx.ReportAnova(the.model))
  lm.out
}

#' @inheritParams createReportLinearOSR
#' @export
#' @rdname createReportLinear
createReportLinearXDF <- function(the.model, config){
  AlteryxMessage2("Creation of the Analysis of Variance table was surpressed due to the use of an XDF file", iType = 2, iPriority = 3)
  lm.out <- AlteryxReportRx(the.model)
  lm.out <- rbind(c("Model_Name", config$`Model Name`), lm.out)
  lm.out
}

#' Create Plots
#'
#' Prepare the basic regression diagnostic plots if it is requested
#' and their isn't the combination of singularities and the use of
#' sampling weights.
#'
#' @param the.model model object
#' @export
createPlotOutputsLinearOSR <- function(the.model){
  par(mfrow=c(2, 2), mar=c(5, 4, 2, 2) + 0.1)
  plot(the.model)
}

#' Plots in XDF
#'
#' @export
createPlotOutputsLinearXDF <- function(){
  noDiagnosticPlot("The diagnostic plot is not available for XDF based models")
}



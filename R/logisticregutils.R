#' ### Define Helper Functions
#'
#'
#' #### Process Inputs
#'
#' These two function take `inputs` and `config` and return the model object
#' along with other elements essential to create the reports and plots
#'
#' __Process inputs for Open Source R__
processOSR <- function(inputs, config){
  var_names <- getNamesFromOrdered(names(inputs$the.data), config$used.weights)

  # Make sure the target is binary
  ylevels <- levels(inputs$the.data[[1]])
  num_levels <- length(unique(ylevels))
  if (num_levels != 2) {
    stop.Alteryx("The target variable must only have two unique values.")
  }

  the.formula <- makeFormula(var_names$x, var_names$y)
  # If sample weights are used
  if (config$used.weights) {
    # Adjust the set of field names to remove the weight field
    # if weights are used
    library(survey)
    model_type <- "quasibinomial"
    the.design <- svydesign(
      ids = ~1, weights = makeFormula(var_names$w, ""), data = inputs$the.data
    )
    the.family <- quasibinomial(config$the.link)
    the.model <- svyglm(the.formula, family = the.family, design = the.design)
  } else {
    model_type <- "binomial"
    the.family <- binomial(config$the.link)
    the.model <- glm(the.formula, family = the.family, data = inputs$the.data)
  }
  list(the.model = the.model, model_type = model_type)
}

#' __Process inputs for XDF__
processXDF <- function(inputs, config){
  var_names <- getNamesFromOrdered(names(inputs$the.data), config$used.weights)

  # Make sure the target is binary
  len.target <- rxGetVarInfo(XDFInfo$xdf_path)[[var_names$y]]$levels
  if(len.target != 2){
    stop.Alteryx2("The target variable must only have two unique values.")
  }

  the.formula <- makeFormula(var_names$x, var_names$y)

  if (config$the.link != "logit"){
    AlteryxMessage2("Only the logit link function is available for XDF files, and will be used.", iType = 2, iPriority = 3)
  }

  # CHECK:
  # 1. is the default for pweights NULL ?
  # 2. does this take a character or a vector ?
  the.model <- rxLogit(formula = the.formula, data = XDFInfo$xdf_path,
    pweights = var_names$w, dropFirst = TRUE
  )
  null.model <- rxLogit(makeFormula("1", var_names$y), data = XDFInfo$xdf_path,
    pweights = var_names$w
  )

  # Add the level labels for the target and predictors, along with
  # the target counts to the model object
  target.info <- rxSummary(makeFormula(var_names$y, ""),
    data = XDFInfo$xdf_path)$categorical
  the.model$yinfo <- list(
    levels = as.character(target.info[[1]][,1]),
    counts = target.info[[1]][,2]
  )
  the.model$xlevels <- getXdfLevels(
    makeFormula(var_names$x, ""), XDFInfo$xdf_path)
  list(the.model = the.model, null.model = null.model)
}

#' #### Create Reports
#'
#' If the ANOVA table is requested then create it and add its results to the
#' key-value table. Its creation will be surpressed if the car package isn't
#' present, if their were singularities in estimation, or if the input is an
#' XDF file.
createReportOSR <- function(the.model, config, model_type) {
  glm.out1 <- Alteryx.ReportGLM(the.model)
  glm.out <- glm.out1$summary.df
  singular <- glm.out1$singular
  glm.out <- rbind(c("Model_Name", config$model.name), glm.out)
  glm.out <- rbind(glm.out, c("Model_Type", model_type))
  if (!singular) {
    glm.out <- rbind(glm.out, Alteryx.ReportAnova(the.model))
  } else {
    AlteryxMessage2("Creation of the Analysis of Deviance table was surpressed due to singularities", iType = 2, iPriority = 3)
  }
}

createReportXDF <- function(the.model, config, null.model, nOutput = 2) {
  glm.out <- AlteryxReportRx(the.model, null.model$deviance)
  glm.out <- rbind(c("Model_Name", config$model.name), glm.out)
  glm.out <- rbind(glm.out, c("Model_Type", "binomial"))
  AlteryxMessage2("Creation of the Analysis of Deviance tables was surpressed due to the use of an XDF file", iType = 2, iPriority = 3)
}

#' #### Create Plots
#'
#' Prepare the basic regression diagnostic plots if it is requested
#' and their isn't the combination of singularities and the use of
#' sampling weights.
#'
#' __Plots in Open Source R__
createPlotOutputsOSR <- function(the.model, singular, config){
  if (!(singular && config$used.weights)) {
    par(mfrow=c(2, 2), mar=c(5, 4, 2, 2) + 0.1)
    plot(the.model)
  } else {
    noDiagnosticPlot("The diagnostic plot is not available due to singularities")
  }
}

#' __Plots in XDF__
createPlotOutputsXDF <- function(){
  noDiagnosticPlot("The diagnostic plot is not available for XDF based models")
}

#' Function to create empty plot with a message
noDiagnosticPlot <- function(msg){
  plot(x = c(0,1), y = c(0,1), type = "n", main = "Plot not available",
    xlab = "", ylab = "", xaxt = "n", yaxt = "n"
  )
  AlteryxMessage2(msg, iType = 2, iPriority = 3)
}

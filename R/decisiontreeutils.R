#' General S3 Method for Validating config
#' Error checking pre-model
#' Does not return anything - just throws errror
#'
#' @param config list of config options
#' @param the.data incoming data
#' @param names list of x, y, w names for data
checkValidConfig <- function(config, the.data, names) {
  UseMethod("checkValidConfig", config)
}

#' Error checking pre-model
#' Does not return anything - just throws errror
#'
#' @param config list of config options
#' @param the.data incoming data
#' @param names list of x, y, w names for data
checkValidConfig.rpart <- function(config, the.data, names) {
  cp <- if (config$cp == "Auto" || config$cp == "") .00001 else config$cp

  target <- the.data[[names$y]]
  if (is.numeric(target) && length(unique(target)) < 5) {
    AlteryxMessage2(
      "The target variable is numeric, however, it has 4 or fewer unique values.",
      iType = 2, iPriority = 3)
  }

  if(cp < 0 || cp > 1) {
    stop.Alteryx2("The complexity parameter must be between 0 and 1. Please try again.")
  }
  if(is.na(as.numeric(cp)) && !(cp == "Auto" || cp == "")) {
    stop.Alteryx2(
      "The complexity parameter provided is not a number. Please enter a new value and try again.")
  }
}

#' Error checking pre-model
#' Does not return anything - just throws errror
#'
#' @param config list of config options
#' @param the.data incoming data
#' @param names list of x, y, w names for data
checkValidConfig.XDF <- function(config, the.data, names) {
  cp <- if (config$cp == "Auto" || config$cp == "") .00001 else config$cp

  if(cp < 0 || cp > 1) {
    stop.Alteryx2("The complexity parameter must be between 0 and 1. Please try again.")
  }
  if(is.na(as.numeric(cp)) && !(cp == "Auto" || cp == "")) {
    stop.Alteryx2(
      "The complexity parameter provided is not a number. Please enter a new value and try again.")
  }
}

#' Error checking pre-model
#' Does not return anything - just throws errror
#'
#' @param config list of config options
#' @param the.data incoming data
#' @param names list of x, y, w names for data
#' @import assertthat
checkValidConfig.C50 <- function(config, the.data, names) {
  library("assertthat")

  # check on trials
  Alteryx_assert(is.boundedInt(config$trials, min = 1),
                 "trials must be a integer with value at least 1"
                 )

  # check on rules
  Alteryx_assert(is.logical(config$rules),
                 "rules must be a boolean value"
                 )

  # check on subset
  Alteryx_assert(is.logical(config$subset),
                 "subset must be a boolean value"
                 )

  # check on bands and bands.check
  Alteryx_assert(is.logical(config$bands.check),
                 "bands.check must be a boolean value"
                 )

  Alteryx_assert(is.boundedInt(config$bands, min = 2, max = 1000),
                 "bands must be integer between 2 and 1000, inclusive"
                 )

  # check on winnow
  Alteryx_assert(is.logical(config$winnow),
                 "winnow must be a boolean value"
                 )

  # check on GlobalPruning
  Alteryx_assert(is.logical(config$GlobalPruning),
                 "GlobalPruning must be a boolean value"
                 )

  # check on CF
  Alteryx_assert(is.boundedReal(config$bands, min = 0, max = 1, closed = FALSE),
                 "bands must be strictly between 0 and 1"
                 )

  # check on minCases
  Alteryx_assert(is.boundedInt(config$minCases, min = 1),
                 "minCases must be a integer with value at least 1"
                 )

  # check on fuzzyThreshold
  Alteryx_assert(is.logical(config$fuzzyThreshold),
                 "fuzzyThreshold must be a boolean value"
                 )

  # check on sample
  Alteryx_assert(is.boundedReal(config$sample, min = 0, max = 1, closed = c(TRUE, FALSE)),
                 "sample must be in range greater than or equal to 0 and strictly less than 1"
                 )

  # check on seed
  Alteryx_assert(is.integerValue(config$seed),
                 "seed must be a integer"
                 )

  # check on earlyStopping
  Alteryx_assert(is.logical(config$earlyStopping),
                 "earlyStopping must be a boolean value"
                 )

  # check on model.algorithm
  Alteryx_assert(config$model.algorithm %in% c("rpart", "C5.0"))

}

#' Error checking pre-model defaults to rpart
#' Does not return anything - just throws errror
#'
#' @param config list of config options
#' @param the.data incoming data
#' @param names list of x, y, w names for data
checkValidConfig.default <- function(config, the.data, names) {
  checkValidConfig.rpart(config, the.data, names)
}

#' Creation of components for model object evaluation
#'
#' @param config list of config options
#' @param names list of variable names (x, y and w)
#' @return list with components needed to create model
createDTParams <- function(config, names) {
  # use lists to hold params
  params <- config[c('minsplit', 'minbucket', 'xval', 'maxdepth',
                     'trials', 'rules', 'subset', 'bands',
                     'bands.check', 'winnow', 'CF', 'minCases',
                     'fuzzyThreshold', 'sample', 'seed', 'earlyStopping'
                     )]
  params <- modifyList(params, list(
    cp = if (config$cp %in% c("Auto", "")) 1e-5 else as.numeric(config$cp),
    data = quote(the.data),
    formula = makeFormula(names$x, names$y),
    weights = names$w
  ))

  # get method and parms params
  if (config$select.type){
    params$method <- if (config$classification) "class" else "anova"
    if (config$classification) {
      params$parms <- list(split = if (config$use.gini) "gini" else "information")
    }
  }

  # get usesurrogate param
  usesurrogate <- config[c('usesurrogate.0', 'usesurrogate.1', 'usesurrogate.2')]
  params$usesurrogate <- which(unlist(usesurrogate, use.names = F)) - 1

  # get max bins param
  params$maxNumBins <- config$maxNumBins

  params$noGlobalPruning <- !(config$GlobalPruning)

  params
}



#' Map parameter names for rpart and XDF
#'
#' @param f_string string of function
#' @param params list of decision tree params
#' @return list with named parameters for f_string
convertDTParamsToArgs <- function(params, f_string) {
  fmap <- list(
    rpart = c(data = "data", formula = "formula", weights = "weights", method = "method",
      parms = "parms", usesurrogate = "usesurrogate", minsplit = "minsplit",
      minbucket = "minbucket", xval = "xval", maxdepth = "maxdepth", cp = "cp"
    ),
    rxDTree = c(data = "data", formula = "formula", weights = "pweights", method = "method",
      parms = "parms", usesurrogate = "useSurrogate", maxNumBins = "maxNumBins",
      minsplit = "minSplit", minbucket = "minBucket", xval = "xVal",
      maxdepth = "maxDepth", cp = "cp"
    ),
    C5.0 = c(data = "data", formula = "formula", trials = "trials", rules = "rules",
      weights = "weights", subset = "subset", bands = "bands", winnow = "winnow",
      noGlobalPruning = "noGlobalPruning", CF = "CF", minCases = "minCases",
      fuzzyThreshold = "fuzzyThreshold", sample = "sample", seed = "seed",
      earlyStopping = "earlyStopping"
    )
  )
  to_rename <- intersect(names(fmap$rpart), names(params))
  plyr::rename(params[to_rename], fmap[[f_string]], warn_missing = F)
}

#' Adjusts config based on results if config was initially "Auto"
#'
#' @param model model object
#' @param config list of config options
#' @return model obj after adjusting complexity parameter
adjustCP <- function(model, config) {
  UseMethod(adjustCP, model)
}

#' Adjusts config based on results if config was initially "Auto"
#'
#' @inheritParams adjustCP
adjustCP.rpart <- function(model, config) {
  if(is.na(as.numeric(config$cp)) && (config$cp == "Auto" || config$cp == "")) {
    cp_table <- as.data.frame(model$cptable)
    pos_cp <- cp_table$CP[(cp_table$xerror - 0.5*cp_table$xstd) <= min(cp_table$xerror)]
    new_cp <- pos_cp[1]
    if (cp_table$xerror[1] == min(cp_table$xerror)) {
      stop.Alteryx2("The minimum cross validation error occurs for a CP value where there are no splits. Specify a complexity parameter and try again.")
    }
    prune(model, cp = new_cp)
  } else {
    model
  }
}

#' Adjusts config based on results if config was initially "Auto"
#'
#' @inheritParams adjustCP
adjustCP.rxDTree <- function(model, config) {
  adjustCP.rpart(model, config)
}

#' Adjusts config based on results if config was initially "Auto"
#'
#' @inheritParams adjustCP
adjustCP.C5.0 <- function(model, config) {
  model
}

#' Adjusts config based on results if config was initially "Auto"
#'
#' @inheritParams adjustCP
adjustCP.default <- function(model, config) {
  adjustCP.rpart(model, config)
}

#' Process DT model
#'
#' @param inputs input data streams to the tool
#' @param config configuration passed to the tool
#' @return list of results or results
#' @import rpart rpart.plot
#' @export
processDT <- function(inputs, config) {
  var_names <- getNamesFromOrdered(names(inputs$the.data), config$used.weights)
  the.data <- inputs$the.data

  checkValidConfig(config, the.data, var_names)

  params <- createDTParams(config, var_names)

  f_string <- config$model.algorithm
  if (inputs$XDFInfo$is_XDF)
    f_string <- 'rxDTree'

  if (inputs$XDFInfo$is_XDF)
    params$data <- inputs$XDFInfo$xdf_path

  args <- convertDTParamsToArgs(params, f_string)

  model <- do.call(f_string, args)

  # Post-model Error checking & cp adjustment if specified to "Auto"
  adjustCP(model, config)
}

#' Get common report objects
#'
#' @param model model object
#' @param out results from printcp
#' @return list of piped results
#' @importFrom magrittr %>% extract
#' @export
getReportObjectDT <- function(model, out) {
  model_sum <- out %>%
    extract(grep("^Variable", .):grep("^n=", .)) %>%
    .[. != ""] %>%
    data.frame(grp = "Model_Sum", out = ., stringsAsFactors = FALSE)

  prune_tbl <- out %>%
    extract((grep("^\\s*CP", .) + 1):length(.)) %>%
    gsub("\\s+", "|", .) %>%
    data.frame(grp = "Prune", out = ., stringsAsFactors = FALSE)

  list(model_sum = model_sum, prune_tbl = prune_tbl)
}

#' Generic S3 class
#' Get data for static report (grp|out pipes)
#'
#' @param model model object
#' @param config list of config options
#' @param names names of variables (x, y and w)
#' @param xdf_path string of xdf file location
#' @return dataframe of piped results
#' @importFrom magrittr %>% extract
createReportDT <- function(model, config = NULL, names = NULL, xdf_path = NULL) {
  UseMethod("createReportDT", model)
}

#' Get data for static report (grp|out pipes) for rpart model
#'
#' @inheritParams createReportDT
#' @return dataframe of piped results
#' @importFrom magrittr %>% extract
createReportDT.rpart <- function(model, config, names, xdf_path) {
  out <- capture.output(printcp(model))
  reportObj <- getReportObjectDT(model, out)

  leaves <- capture.output(model) %>%
    extract(grep("^node", .):length(.)) %>%
    gsub(">", "&gt;", .) %>%
    gsub("<", "&lt;", .) %>%
    gsub("\\s", "<nbsp/>", .) %>%
    data.frame(grp = "Leaves", out = ., stringsAsFactors = FALSE)

  call <- capture.output(model$call) %>%
    paste(., collapse = "") %>%
    data.frame(grp = "Call", out = ., stringsAsFactors = FALSE)

  rpart_out <- rbind(
    c("Model_Name", config$model.name),
    call, reportObj$model_sum, reportObj$prune_tbl, leaves,
    c("Model_Class", 'rpart')
  )

  list(out = rpart_out, model = model)
}

#' Get data for static report (grp|out pipes) for rxDTree model
#'
#' @inheritParams createReportDT
#' @return dataframe of piped results
#' @importFrom magrittr %>% extract
createReportDT.rxDTree <- function(model, config, names, xdf_path) {
  model_rpart <- RevoScaleR::rxAddInheritance(model)
  printcp(model_rpart)

  out <- capture.output(printcp(model_rpart))

  model$xlevels <- do.call(match.fun("getXdfLevels"),
                           list(formula = as.formula(paste0("~ ", paste(names$x, collapse = " + "))), xdf = xdf_path))

  target_all_data <- RevoScaleR::rxSummary(makeFormula(names$y, ""), data = xdf_path)
  if (target_all_data$categorical.type == "none") {
    target_info <- target_all_data$categorical
    if(length(target_info) == 1) {
      model$yinfo <- list(
        levels = as.character(target_info[[1]][,1]), counts = target_info[[1]][,2])
    }
  }

  call <- capture.output(model$call) %>%
    paste(., collapse = "") %>%
    gsub("xdf_path", xdf_path, .) %>%
    data.frame(grp = "Call", out = ., stringsAsFactors = FALSE)

  leaves <- capture.output(model_rpart) %>%
    extract(grep("^node", .):length(.)) %>%
    gsub(">", "&gt;", .) %>%
    gsub("<", "&lt;", .) %>%
    gsub("\\s", "<nbsp/>", .) %>%
    data.frame(grp = "Leaves", out = ., stringsAsFactors = FALSE)

  reportObj <- getReportObjectDT(model_rpart, out)

  rpart_out <- rbind(
    c("Model_Name", config$model.name),
    call, reportObj$model_sum, reportObj$prune_tbl, leaves,
    c("Model_Class", 'rxDTree')
  )
  list(out = rpart_out, model = model, model_rpart = model_rpart)
}

#' Get data for static report for C5.0 model
#'
#' @inheritParams createReportDT
#' @return dataframe of piped results
createReportDT.C5.0 <- function(model, config, name, xdf_path) {
  list(out = capture.output(summary(model)), model = model, model_rpart = NULL)
}

#' Create Tree Plot
#'
#' @param model model object
#' @param config configuration object
#' @return graphs
#' @export
createTreePlotDT <- function(model, config) {
  UseMethod("creatTreePlotDT", model)
}

#' Create Tree Plot for rpart model
#'
#' @inheritParams createTreePlotDT
#' @return graphs
#' @export
createTreePlotDT.rpart <- function(model, config) {
  leaf_sum <- if (model$method != "class") 0 else if (config$do.counts == TRUE) 2 else 4
  uniform <- config$b.dist
  fallen <- !uniform
  par(mar = c(5, 4, 6, 2) + 0.1)
  rpart.plot::rpart.plot(
    model, type = 0, extra = leaf_sum, uniform = uniform, fallen.leaves = fallen,
    main = "Tree Plot", cex = 1
  )
}

#' Create Tree Plot for rxDTree model
#'
#' @inheritParams createTreePlotDT
#' @return graphs
#' @export
createTreePlotDT.rxDTree <- function(model, config) {
  createTreePlotDT.rpart(model, config)
}

#' Create Tree Plot for C5.0 model
#'
#' @inheritParams createTreePlotDT
#' @return graphs
#' @export
createTreePlotDT.C5.0 <- function(model, config) {
  par(mar = c(5, 4, 6, 2) + 0.1)
  plot(model, trials = config$trials)
}

#' Create Tree Plot for C5.0 model
#'
#' @inheritParams createTreePlotDT
#' @return graphs
#' @export
createTreePlotDT.default <- function(model, config) {
  par(mar = c(5, 4, 6, 2) + 0.1)
  plot(model)
}

#' Create Prune Plot
#'
#' @param model model object
createPrunePlotDT <- function(model){
  par(mar = c(5, 4, 6, 2) + 0.1)
  rpart::plotcp(model, main = NULL)
  title(main = "Pruning Plot", line=5)
}

#' Generic S3 Class
#' Create Interactive Dashboard
#'
#' @param model model object
#' @import htmltools
createDashboardDT <- function(model) {
  UseMethod("createDashboardDT", model)
}

#' Create Interactive Dashboard for rpart model
#'
#' @param model model object
#' @import htmltools
createDashboardDT.rpart <- function(model) {
  if (!(packageVersion('AlteryxRviz') >= "0.2.5")){
    k1 = tags$div(
      tags$h4("You need AlteryxRviz >= 0.2.5")
    )
  } else {
    #model = rpart(Species ~ ., data = iris)
    tooltipParams = list(
      width = '250px',
      top = '130px',
      left = '100px'
    )
    dt = AlteryxRviz::renderTree(model, tooltipParams = tooltipParams)
    vimp = AlteryxRviz::varImpPlot(model, height = 300)

    cmat = if (!is.null(model$frame$yval2)){
      AlteryxRviz::iConfusionMatrix(AlteryxRviz::getConfMatrix(model), height = 300)
    }  else {
      tags$div(h1('Confusion Matrix Not Valid'), height = 300)
    }

    k1 = AlteryxRviz::dtDashboard(dt, vimp, cmat)
  }

  k1
}

#' Create Interactive Dashboard for default model
#'
#' @inheritParams createDashboardDT
#' @import htmltools
createDashboardDT.rxDTree <- function(model) {
  k1 = tags$div(tags$h4(
    "Interactive Visualizations are not supported for Revolution R Enterprise"
  ))

  k1
}

#' Create Interactive Dashboard for default model
#'
#' @inheritParams createDashboardDT
#' @import htmltools
createDashboardDT.default <- function(model) {
  k1 = tags$div(tags$h4(
    paste0("Interactive Visualizations are not supported for ", class(model)[1], " model types")
  ))

  k1
}

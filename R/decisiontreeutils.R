#' General Method for Validating config
#' Error checking pre-model
#' Returns a vector of Booleans indicating warnings and errors
#'
#' @param config list of config options
#' @param the.data incoming data
#' @param names list of x, y, w names for data
#' @return messages_vb vector of booleans
checkValidConfig <- function(config, the.data, names) {
  # To accomodate the tests in test-checkValidConfig, the various checkValidConfig
  # child functions will create boolean vectors where each element indicates
  # whether a particular error/warning is generated.
  if (config$model.algorithm == "C5.0") {
    messages_vb <- checkValidConfigC5.0(config, the.data, names)
  }
  else if (config$model.algorithm == "rxDTree") {
    messages_vb <- checkValidConfigrxDTree(config, the.data, names)
  }
  else {
    messages_vb <- checkValidConfigrpart(config, the.data, names)
  }
  return(messages_vb)
}

#' Outputting the warnings/errors.
#' This function was originally written to pair with checkValidConfig,
#' but it is extensible: you can call it anytime you need to write out a
#' concatenated warning and/or error string. All three arguments must be
#' vectors of the same length.
#' Does not return anything. It only writes out concatenated errors/warnings.
#' @param in.messages_vb vector of Booleans indicating which errors/warnings to display
#' @param in.informationalMessageWarningOrErrorIndicator_vc vector of strings indicating whether the message should be displayed as an informational message ("message"), warning ("warning"), or error ("error")
#' @param in.messageText_vc vector of strings to be displayed to the user.
display_config_messages <- function(
  in.messages_vb,
  in.informationalMessageWarningOrErrorIndicator_vc,
  in.messageText_vc
  ) {
  informationalMessages_vi <- which(
    in.informationalMessageWarningOrErrorIndicator_vc == "message"
  )
  displayedInformationalMessages_vi <- intersect(
    informationalMessages_vi,
    which(in.messages_vb)
  )
  if (length(displayedInformationalMessages_vi) > 0) {
    fullMessage_sc <- ""
    # A for loop may not be the most elegant solution here.
    # It's the best one I can come up with for now, since paste0 doesn't
    # paste together vector elements.
    # If we end up using this function with lots of messages, this could be
    # ripe for re-factoring.
    # But for now, I don't foresee speed being a huge issue here.
    for (i in 1 : length(displayedInformationalMessages_vi)) {
      fullMessage_sc <- paste0(
        fullMessage_sc,
        " ",
        in.messageText_vc[displayedInformationalMessages_vi[i]]
      )
    }
    AlteryxMessage2(
      msg = fullMessage_sc,
      1,
      3
    )
  }

  warnings_vi <- which(
    in.informationalMessageWarningOrErrorIndicator_vc == "warning"
  )
  displayedWarnings_vi <- intersect(
    warnings_vi,
    which(in.messages_vb)
  )
  if (length(displayedWarnings_vi) > 0) {
    fullMessage_sc <- ""
    for (i in 1 : length(displayedWarnings_vi)) {
      fullMessage_sc <- paste0(
        fullMessage_sc,
        " ",
        in.messageText_vc[displayedWarnings_vi[i]]
      )
    }
    AlteryxMessage2(
      msg = fullMessage_sc,
      2,
      3
    )
  }

  errors_vi <- which(
    in.informationalMessageWarningOrErrorIndicator_vc == "error"
  )
  displayedErrors_vi <- intersect(
    errors_vi,
    which(in.messages_vb)
  )
  if (length(displayedErrors_vi) > 0) {
    fullMessage_sc <- ""
    for (i in 1 : length(displayedErrors_vi)) {
      fullMessage_sc <- paste0(
        fullMessage_sc,
        " ",
        in.messageText_vc[displayedErrors_vi[i]]
      )
    }
    stop.Alteryx2(
      msg = fullMessage_sc
    )
  }
}

#' Error checking pre-model
#' Does not return anything - just throws errror
#'
#' @param config list of config options
#' @param the.data incoming data
#' @param names list of x, y, w names for data
checkValidConfigrpart <- function(config, the.data, names) {
  # checkLowN will not be part of messages_vb, because writing out the low n warning
  # would require re-factoring the function, and the function is used elsewhere
  # in the codebase (at least directly in the Linear Regression code, and
  # potentially elsewhere).
  checkLowN(
    data = the.data,
    threshold = 25,
    mult = 1,
    msg = XMSG(
      in.targetString_sc = "The incoming data may not have enough rows to generate a model succesfully."
    )
  )

  cp <- if (config$cp == "Auto" || config$cp == "") {
    .00001
    } else {
      config$cp
    }

  target <- the.data[[names$y]]
  messages_vb <- rep_len(
    x = FALSE,
    length.out = 18
  )
  if (is.numeric(target) && length(unique(target)) < 5) {
    messages_vb[1] <- TRUE
  }
  if(cp < 0 || cp > 1) {
    messages_vb[2] <- TRUE
  }
  if(is.na(as.numeric(cp)) && !(cp == "Auto" || cp == "")) {
    messages_vb[3] <- TRUE
  }
  return(messages_vb)
}

#' Error checking pre-model
#' Does not return anything - just throws errror
#'
#' @param config list of config options
#' @param the.data incoming data
#' @param names list of x, y, w names for data
checkValidConfigrxDTree <- function(config, the.data, names) {
  cp <- if (config$cp == "Auto" || config$cp == "") .00001 else config$cp
  messages_vb <- rep_len(
    x = FALSE,
    length.out = 18
  )
  if(cp < 0 || cp > 1) {
    messages_vb[2] <- TRUE

  }
  if(is.na(as.numeric(cp)) && !(cp == "Auto" || cp == "")) {
    messages_vb[3] <- TRUE
  }
  return(messages_vb)
}

#' Error checking pre-model
#' Does not return anything - just throws errror
#'
#' @param config list of config options
#' @param the.data incoming data
#' @param names list of x, y, w names for data
#' @import assertthat
checkValidConfigC5.0 <- function(config, the.data, names) {
  checkLowN(
    data = the.data,
    threshold = 25,
    mult = 1 - config$sample,
    msg = XMSG(
      in.targetString_sc = "The incoming data may not have enough data to generate a model succesfully. Consider holding out less data for model evaluation."
    )
  )
  messages_vb <- rep_len(
    x = FALSE,
    length.out = 18
  )
  # check on trials
  if (!is.boundedInt(config$trials, min = 1)) {
    messages_vb[4] <- TRUE
  }

  # check on rules
  if (!is.logical(config$rules)) {
    messages_vb[5] <- TRUE
  }

  # check on subset
  if (!is.logical(config$subset)) {
    messages_vb[6] <- TRUE
  }

  # check on bands and bands.check
  if (!is.logical(config$bands.check)) {
    messages_vb[7] <- TRUE
  }

  if (!is.boundedInt(config$bands, min = 0, max = 1000)) {
    messages_vb[8] <- TRUE
  }

  if (config$bands == 1) {
    messages_vb[9] <- TRUE
  }


  # check on winnow
  if (!is.logical(config$winnow)) {
    messages_vb[10] <- TRUE
  }

  # check on GlobalPruning
  if (!is.logical(config$GlobalPruning)) {
    messages_vb[11] <- TRUE
  }

  # check on CF
  if (!is.boundedReal(config$CF, min = 0, max = 1, closed = FALSE)) {
    messages_vb[12] <- TRUE
  }

  # check on minCases
  if (!is.boundedInt(config$minCases, min = 1)) {
    messages_vb[13] <- TRUE
  }

  # check on fuzzyThreshold
  if (!is.logical(config$fuzzyThreshold)) {
    messages_vb[14] <- TRUE
  }


  # check on sample
  if (
    !is.boundedReal(
      config$sample,
      min = 0,
      max = 1,
      closed = c(TRUE, FALSE)
    )
  ) {
    messages_vb[15] <- TRUE
  }

  # check on seed
  if (!is.integerValue(config$seed)) {
    messages_vb[16] <- TRUE
  }

  # check on earlyStopping
  if (!is.logical(config$earlyStopping)) {
    messages_vb[17] <- TRUE
  }

  # check on model.algorithm
  if (!(config$model.algorithm %in% c("rpart", "C5.0", "rxDTree"))) {
    messages_vb[18] <- TRUE
  }
  return(messages_vb)
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
                     'winnow', 'CF', 'minCases',
                     'fuzzyThreshold', 'sample', 'seed', 'earlyStopping'
                     )]
  params <- modifyList(params, list(
    cp = if (config$cp %in% c("Auto", "")) 1e-5 else as.numeric(config$cp),
    data = quote(the.data),
    formula = makeFormula(names$x, names$y),
    weights = if (is.null(names$w)) NULL else as.symbol(names$w)
  ))
  if (params$cp == 1) {
    params$cp <- .999
    AlteryxMessage2(XMSG(in.targetString_sc = "Control Parameter of 1 is not permitted."))
    AlteryxMessage2(XMSG(in.targetString_sc = "Control Parameter was converted to .999"))
  }

  if (!config$bands.check)
    params$bands <- 0

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
  params$maxNumBins <- as.numeric(config$maxNumBins)


  params$noGlobalPruning <- !(config$GlobalPruning)

  params$surrogatestyle <- if (config$total.correct) 0 else 1

  params
}

#' Map parameter names to function argument names
#'
#' @param params list of decision tree params
#' @param model.algorithm model function to call
#' @return list with named parameters  for function
convertDTParamsToArgs <- function(params, model.algorithm) {
  if (model.algorithm == "C5.0")
    convertDTParamsToArgsC5.0(params)
  else if(model.algorithm == "rxDTree")
    convertDTParamsToArgsrxDTree(params)
  else
    convertDTParamsToArgsrpart(params)
}

#' Map parameter names to function arg names for rpart
#'
#' @param params list of decision tree params
#' @return list with named parameters for rpart
convertDTParamsToArgsrpart <- function(params) {
  arg_names <- c("formula", "data", "weights", "method", "parms", "minsplit",
                 "minbucket", "cp", "usesurrogate", "maxdepth", "xval",
                 "surrogatestyle")

  params[intersect(names(params), arg_names)]
}

#' Map parameter names to function arg names for rxDTree
#'
#' @param params list of decision tree params
#' @return list with named parameters for rxDTree
convertDTParamsToArgsrxDTree <- function(params) {

  params_rpart <- c("formula", "data", "weights", "method", "parms",
                    "minsplit", "minbucket", "cp", "usesurrogate",
                    "maxdepth", "xval", "surrogatestyle", "maxNumBins")

  args_rpart <- params[intersect(names(params), params_rpart)]
  args_rpart$weights <- if(is.null(args_rpart$weights)) NULL else as.character(args_rpart$weights)
  plyr::rename(args_rpart, c("weights" = "pweights",
                             "usesurrogate" = "useSurrogate",
                             "minsplit" = "minSplit",
                             "minbucket" = "minBucket",
                             "xval" = "xVal",
                             "maxdepth" = "maxDepth",
                             "surrogatestyle" = "surrogateStyle"
  ),
  warn_missing = FALSE)
}

#' Map parameter names to function arg names for C5.0
#'
#' @param params list of decision tree params
#' @return list with named parameters for C5.0
convertDTParamsToArgsC5.0 <- function(params) {
  arg_names <- c("formula", "data", "trials", "rules", "weights")
  control_names <- c("subset", "bands", "winnow", "noGlobalPruning", "CF",
                     "minCases", "fuzzyThreshold", "sample", "seed",
                     "earlyStoping")

  control <- params[intersect(control_names, names(params))]
  args <- params[intersect(arg_names, names(params))]

  args$control <- do.call(C5.0Control, control)
  args
}

#' Adjusts config based on results if config was initially "Auto"
#'
#' @param model model object
#' @param config list of config options
#' @return model obj after adjusting complexity parameter
adjustCP <- function(model, config) {
  UseMethod("adjustCP", model)
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
      stop.Alteryx2(
        XMSG(
          in.targetString_sc = "The minimum cross validation error occurs for a CP value where there are no splits. Specify a complexity parameter and try again."
        )
      )
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
#' @import C50
#' @export
processDT <- function(inputs, config) {
  var_names <- getNamesFromOrdered(names(inputs$the.data), config$used.weights)
  the.data <- inputs$the.data

  messages_vb <- checkValidConfig(config, the.data, var_names)
  display_config_messages(
    in.messages_vb = messages_vb,
    in.informationalMessageWarningOrErrorIndicator_vc = c(
      "warning",
      rep_len(
        x = 'error',
        length.out = 17
      )
    ),
    in.messageText_vc = c(
      XMSG(
        in.targetString_sc = "The target variable is numeric, however, it has 4 or fewer unique values."
        ),
      XMSG(
        in.targetString_sc = "The complexity parameter must be between 0 and 1. Please try again."
        ),
      XMSG(
        in.targetString_sc = "The complexity parameter provided is not a number. Please enter a new value and try again."
      ),
      XMSG(
        in.targetString_sc = "trials must be a integer with value at least 1."
      ),
      XMSG(in.targetString_sc = "rules must be a boolean value."),
      XMSG(in.targetString_sc = "subset must be a boolean value."),
      XMSG(in.targetString_sc = "bands.check must be a boolean value."),
      XMSG(in.targetString_sc = "bands must be integer between 0 and 1000, inclusive."),
      XMSG(in.targetString_sc = "bands must be integer between 0 and 1000, inclusive."),
      XMSG(in.targetString_sc = "winnow must be a boolean value."),
      XMSG(in.targetString_sc = "GlobalPruning must be a boolean value."),
      XMSG(in.targetString_sc = "CF must be strictly between 0 and 1."),
      XMSG(
        in.targetString_sc = "minCases must be a integer with value at least 1"
        ),
      XMSG(in.targetString_sc = "fuzzyThreshold must be a boolean value"),
      XMSG(
        in.targetString_sc = "sample must be in range greater than or equal to 0 and strictly less than 1"
      ),
      XMSG(in.targetString_sc = "seed must be a integer"),
      XMSG(in.targetString_sc = "earlyStopping must be a boolean value"),
      XMSG(in.targetString_sc = "model.algorithm must be rpart, C5.0 or rxDTree")
    )
  )

  params <- createDTParams(config, var_names)

  if (inputs$XDFInfo$is_XDF)
    params$data <- inputs$XDFInfo$xdf_path

  args <- convertDTParamsToArgs(params, config$model.algorithm)
  model <- do.call(config$model.algorithm, args)

  if(config$model.algorithm == "C5.0") {
    model$y <- the.data[[var_names$y]]
    model$yvar <- var_names$y
    model$xlevels <- lapply(X = the.data[var_names$x], FUN = levels)
    model$ylevels <- levels(the.data[[var_names$y]])
  }


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
createReportDT.C5.0 <- function(model, config, names, xdf_path) {
  list(out = data.frame(grp = "summary", out = capture.output(summary(model))),
         model = model, model_rpart = model
  )
}

#' Create Tree Plot
#'
#' @param model model object
#' @param config configuration object
#' @param inputs input list
#' @return graphs
#' @export
createTreePlotDT <- function(model, config, inputs) {
  UseMethod("createTreePlotDT", model)
}

#' Create Tree Plot for rpart model
#'
#' @inheritParams createTreePlotDT
#' @return graphs
#' @export
createTreePlotDT.rpart <- function(model, config, inputs) {
  leaf_sum <- if (model$method != "class") 0 else if (config$do.counts == TRUE) 2 else 4
  uniform <- config$b.dist
  fallen <- !uniform
  par(mar = c(5, 4, 6, 2) + 0.1)
  rpart.plot::rpart.plot(
    model,
    type = 0,
    extra = leaf_sum,
    uniform = uniform,
    fallen.leaves = fallen,
    main = XMSG(in.targetString_sc = "Tree Plot"),
    cex = 1
  )
}

#' Create Tree Plot for rxDTree model
#'
#' @inheritParams createTreePlotDT
#' @return graphs
#' @export
createTreePlotDT.rxDTree <- function(model, config, inputs) {
  createTreePlotDT.rpart(model, config, inputs)
}

#' Create Tree Plot for C5.0 model
#'
#' @inheritParams createTreePlotDT
#' @return graphs
#' @export
createTreePlotDT.C5.0 <- function(model, config, inputs) {
  # WORKAROUND: The global assignment allows access to the.data by plotting
  #   elements that otherwise cannot find it and error out. This is exlusive
  #   to C5.0 models.
  the.data <<- inputs$the.data
  par(mar = c(5, 4, 6, 2) + 0.1)
  plot(model, trial = config$trials - 1)
}

#' Create Tree Plot for C5.0 model
#'
#' @inheritParams createTreePlotDT
#' @return graphs
#' @export
createTreePlotDT.default <- function(model, config, inputs) {
  par(mar = c(5, 4, 6, 2) + 0.1)
  plot(model)
}

#' Create Prune Plot
#'
#' @param model model object
createPrunePlotDT <- function(model) {
  UseMethod("createPrunePlotDT", model)
}

#' Create Prune Plot
#'
#' @inheritParams createPrunePlotDT
createPrunePlotDT.rpart <- function(model) {
  par(mar = c(5, 4, 6, 2) + 0.1)
  rpart::plotcp(model, main = NULL)
  title(
    main = XMSG(in.targetString_sc = "Pruning Plot"),
    line=5
  )
}

#' Create Prune Plot
#'
#' @inheritParams createPrunePlotDT
createPrunePlotDT.rxDTree <- function(model) {
  createPrunePlotDT.rpart(model)
}

#' Create Prune Plot
#'
#' @inheritParams createPrunePlotDT
createPrunePlotDT.C5.0 <- function(model) {
  NULL
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
      tags$h4(
        XMSG(in.targetString_sc = "You need AlteryxRviz >= 0.2.5")
      )
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
      tags$div(
        h1(
          XMSG(in.targetString_sc = 'Confusion Matrix Not Valid')
        ),
        height = 300
      )
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
  k1 = tags$div(
    tags$h4(
      XMSG(
        in.targetString_sc = "Interactive Visualizations are not supported for Revolution R Enterprise"
      )
    )
  )
  return(k1)
}

#' Create Interactive Dashboard for default model
#'
#' @inheritParams createDashboardDT
#' @import htmltools
createDashboardDT.default <- function(model) {
  k1 = tags$div(
    tags$h4(
      XMSG(
        in.targetString_sc = "Interactive Visualizations are not supported for @1 model types",
        class(model)[1]
      )
    )
  )
  return(k1)
}

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
checkValidConfig.OSR <- function(config, the.data, names) {
  cp <- if (config$cp == "Auto" || config$cp == "") .00001 else config$cp

  target <- the.data[[names$y]]
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

  target <- the.data[[names$y]]
  if (is.numeric(target) && length(unique(target)) < 5 && !is_XDF) {
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


#' Creation of components for model object evaluation
#'
#' @param config list of config options
#' @param names list of variable names (x, y and w)
#' @param xdf_properties list of xdf details (is_XDF and xdf_path elements)
#' @return list with components needed to create model
createDTParams <- function(config, names) {
  # use lists to hold params for rpart and rxDTree functions
  params <- config[c('minsplit', 'minbucket', 'xval', 'maxdepth')]
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

  params
}



#' Map parameter names for OSR and XDF
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
    rxDTree = c(xdf_path = "data", formula = "formula", weights = "pweights", method = "method",
      parms = "parms", usesurrogate = "useSurrogate", maxNumBins = "maxNumBins",
      minsplit = "minSplit", minbucket = "minBucket", xval = "xVal",
      maxdepth = "maxDepth", cp = "cp"
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
  if(is.na(as.numeric(config$cp)) && (config$cp == "Auto" || config$cp == "")) {
    cp_table <- as.data.frame(model$cptable)
    pos_cp <- cp_table$CP[(cp_table$xerror - 0.5*cp_table$xstd) <= min(cp_table$xerror)]
    new_cp <- pos_cp[1]
    #print(cp_table)
    if (cp_table$xerror[1] == min(cp_table$xerror)) {
      stop.Alteryx2("The minimum cross validation error occurs for a CP value where there are no splits. Specify a complexity parameter and try again.")
    }
    prune(model, cp = new_cp)
  } else {
    model
  }
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
  class(config) <- if(inputs$XDFInfo$is_XDF) c("XDF", class(config)) else c("OSR", class(config))

  checkValidConfig(config, the.data, var_names)

  params <- createDTParams(config, var_names)
  f_string <- if (inputs$XDFInfo$is_XDF) 'rxDTree' else 'rpart'

  args <- convertDTParamsToArgs(params, f_string)
  model <- do.call(f_string, args)

  # Post-model Error checking & cp adjustment if specified to "Auto"
  adjustCP(model, config)
}


#' Get data for static report (grp|out pipes)
#'
#' @param model model object
#' @param config list of config options
#' @param names names of variables (x, y and w)
#' @param is_XDF boolean of whether model is XDF
#' @return dataframe of piped results
#' @importFrom magrittr %>% extract
createReportDT <- function(model, config, names, is_XDF) {
  # The output: Start with the pruning table (have rxDTree objects add rpart
  # inheritance for printing and plotting purposes).
  if (is_XDF) {
    # model_rpart <- rxAddInheritance(model)
    # printcp(model_rpart)
    # out <- capture.output(printcp(model_rpart))
    # model$xlevels <- do.call(match.fun("xdfLevels"),
    #   list(paste0("~ ", paste(names$x, collapse = " + ")), xdf_path))
    # if (is.factor(target)) {
    #   target_info <- do.call(match.fun("rxSummary"),
    #     list(paste0("~ ", names$y), data = xdf.path))[["categorical"]]
    #   if(length(target_info) == 1) {
    #     model$yinfo <- list(
    #       levels = as.character(target_info[[1]][,1]), counts = target_info[[1]][,2])
    #   }
    # }
  } else {
    out <- capture.output(printcp(model))
  }

  model_sum <- out %>%
    extract(grep("^Variable", .):grep("^n=", .)) %>%
    .[. != ""] %>%
    data.frame(grp = "Model_Sum", out = ., stringsAsFactors = FALSE)

  model_call <- out %>%
    extract(grep("^r", .):(grep("^Variable", .) - 1)) %>%
    .[. != ""] %>%
    paste(collapse = "") %>%
    data.frame(grp = "Call", out = ., stringsAsFactors = FALSE)

  prune_tbl <- out %>%
    extract((grep("^\\s*CP", .) + 1):length(.)) %>%
    gsub("\\s+", "|", .) %>%
    data.frame(grp = "Prune", out = ., stringsAsFactors = FALSE)

  # Uncomment after fixing XDF code.
  # model <- if (is_XDF) model_rpart else model

  leaves <- capture.output(model) %>%
    extract(grep("^node", .):length(.)) %>%
    gsub(">", "&gt;", .) %>%
    gsub("<", "&lt;", .) %>%
    gsub("\\s", "<nbsp/>", .) %>%
    data.frame(grp = "Leaves", out = ., stringsAsFactors = FALSE)

  # Indicate that this is an object of class rpart or rxDTree
  rpart_out <- rbind(
    c("Model_Name", config$model.name),
    model_call, model_sum, prune_tbl, leaves,
    c("Model_Class", if (is_XDF) 'rxDTree' else 'rpart')
  )
  rpart_out
}

#' Create Tree Plot
#'
#' @param model model object
#' @param config configuration object
#' @export
createTreePlotDT <- function(model, config){
  leaf_sum <- if (model$method != "class") 0 else if (config$do.counts == TRUE) 2 else 4
  uniform <- config$b.dist
  fallen <- !uniform
  par(mar = c(5, 4, 6, 2) + 0.1)
  rpart.plot::rpart.plot(
    model, type = 0, extra = leaf_sum, uniform = uniform, fallen.leaves = fallen,
    main = "Tree Plot", cex = 1
  )
}

#' Create Prune Plot
#'
#' @param model model object
createPrunePlotDT <- function(model){
  par(mar = c(5, 4, 6, 2) + 0.1)
  rpart::plotcp(model, main = NULL)
  title(main = "Pruning Plot", line=5)
}

#' Create Interactive Dashboard
#'
#' @param model model object
#' @param is_XDF boolean of whether model is XDF
#' @import htmltools
createDashboardDT <- function(model, is_XDF) {

  ## Interactive Visualization
  if (is_XDF){
    k1 = tags$div(tags$h4(
      "Interactive Visualizations are not supported for Revolution Enterprise"
    ))
    # renderInComposer(k1, nOutput = 5)
  } else {
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
  }
  k1
}

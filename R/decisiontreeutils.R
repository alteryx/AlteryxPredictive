#' Error checking pre-model
#' Does not return anything - just throws errror
#'
#' @param config list of config options
#' @param the.data incoming data
checkValidConfig <- function(config, the.data) {
  data_names <- names(the.data)
  names <- getNamesFromOrdered(config$used.weights, data_names)
  name_y_var <- names$y
  cp <- ifelse(config$cp == "Auto" || config$cp == "", .00001, config$cp)


  target <- the.data[[name_y_var]]
  if (is.numeric(target) && length(unique(target)) < 5 && !is_XDF) {
    AlteryxMessage2("The target variable is numeric, however, it has 4 or fewer unique values.", iType = 2, iPriority = 3)
  }

  if(rpart_params$cp < 0 || rpart_params$cp > 1) {
    stop.Alteryx2("The complexity parameter must be between 0 and 1. Please try again.")
  }

  if(is.na(as.numeric(config$cp)) && !(config$cp == "Auto" || config$cp == "")) {
    stop.Alteryx2("The complexity parameter provided is not a number. Please enter a new value and try again.")
  }
}


#' Creation of components for model object evaluation
#'
#' @param config list of config options
#' @param data list of datastream inputs
#' @return list with components needed to create model
createDTParams <- function(config, data) {
  # use lists to hold params for rpart and rxDTree functions
  params <- getXdfProperties("#1")

  # get data param
  the.data <- data$data_stream1
  data_names <- names(the.data)
  params$data <- quote(the.data)

  # Get the field names
  names <- getNamesFromOrdered(config$used.weights, data_names)
  name_y_var <- names$y
  names_x_vars <- names$x
  name_weight_var <- names$w

  # use field names to get formula param
  params$formula <- makeFormula(names_x_vars, name_y_var)

  # get weights param
  params$weights <- ifelse(config$used.weights, name_weight_var, NULL)
  rpart_params$weights <- rxDTree_params$pweights <- weights

  # get method and parms params
  if(config$select.type) {
    if(config$classification) {
      params$method <- "class"
      if(config$use.gini) {
        params$parms <- quote(list(split = "gini"))
      } else {
        params$parms <- quote(list(split = "information"))
      }
    } else {
      params$method <- "anova"
    }
  }

  usesurrogate <- config[c('use.surrogate.0', 'use.surrogate.1', 'use.surrogate.2')]
  param_list$usesurrogate <- which(usesurrogate) - 1

  # get max bins param
  if(is_XDF && !is.na(as.numeric(config$maxNumBins))) {
    maxNumBins <- config$maxNumBins
    if(maxNumBins < 2) {
      stop.Alteryx2("The minimum bins is 2")
    } else {
      params$maxNumBins <- maxNumBins
    }
  }

  # other parameters
  params$minsplit <- config$minsplit
  params$minbucket <- config$minbucket
  params$xval <- config$xval
  params$maxdepth <- config$maxdepth

  params$cp <- ifelse(config$cp == "Auto" || config$cp == "", .00001, config$cp)

  params
}

#' name mapping from parameters to functions
#'
#' @param f_string string of function
#' @param params list of decision tree params
#' @return list with named parameters for f_string
convertParamsToArgs <- function(f_string, params) {
  if (f_string == "rpart") {
    list(
      data = params$data,
      formula = params$formula,
      weights = params$weights,
      method = params$method,
      parms = params$parms,
      usesurrogate = params$usesurrogate,
      minsplit <- params$minsplit,
      minbucket = params$minbucket,
      xval = params$xval,
      maxdepth = params$maxdepth,
      cp = params$cp
    )
  } else if(f_string == "rxDTree") {
    list(
      data = quote(xdf_path),
      formula = params$f,
      pweights = params$weights,
      method = params$method,
      parms = params$parms,
      useSurrogate = params$surrogate,
      maxNumBins = params$maxNumBins,
      minSplit <- params$minsplit,
      minBucket = params$minbucket,
      xVal = params$xval,
      maxDepth = params$maxdepth,
      cp = params$cp
    )
  } else {
    stop.Alteryx2(paste("Unsupported function specified: ", f_string))
  }
}

#' adjusts config based on results if config was initially "Auto"
#'
#' @param config list of config options
#' @param model model object
#' @return model obj after adjusting complexity parameter
adjustCP <- function(config, model) {
  if(is.na(as.numeric(config$cp)) && (config$cp == "Auto" || config$cp == "")) {
    cp_table <- as.data.frame(model$cptable)
    pos_cp <- cp_table$CP[(cp_table$xerror - 0.5*cp_table$xstd) <= min(cp_table$xerror)]
    new_cp <- pos_cp[1]
    print(cp_table)
    if (cp_table$xerror[1] == min(cp_table$xerror)) {
      stop.Alteryx2("The minimum cross validation error occurs for a CP value where there are no splits. Specify a complexity parameter and try again.")
    }
    prune(model, cp = new_cp)
  } else {
    model
  }
}

#' get grp|out pipes for outputting static report
#'
#' @param config list of config options
#' @param model model object
#' @param is_XDF boolean of whether model is XDF
#' @return dataframe of piped results
getDTPipes <- function(config, model, is_XDF) {

  # The output: Start with the pruning table (have rxDTree objects add rpart
  # inheritance for printing and plotting purposes).
  if (is_XDF) {
    model_rpart <- rxAddInheritance(model)
    printcp(model_rpart)
    out <- capture.output(printcp(model_rpart))
    model$xlevels <- do.call(match.fun("xdfLevels"), list(paste0("~ ", paste(names_x_vars, collapse = " + ")), xdf_path))
    if (is.factor(target)) {
      target_info <- do.call(match.fun("rxSummary"), list(paste0("~ ", name_y_var), data = xdf.path))[["categorical"]]
      if(length(target_info) == 1) {
        model$yinfo <- list(levels = as.character(target_info[[1]][,1]), counts = target_info[[1]][,2])
      }
    }
  } else {
    printcp(model) # Pruning Table
    out <- capture.output(printcp(model))
  }

  out <- out[out != ""]
  print(out)
  out <- out[2:length(out)]
  # Find the index of the end of the call
  for (i in 1:length(out)) {
    if (grepl("Variables", out[i])) {
      end_call <- i - 1
      break
    }
  }
  call1 <- out[1:end_call]
  call = gsub("\\s\\s", "", paste(call1, collapse=""))
  # Figure out where the summary information ends and the actual pruning table
  # starts. The header for the pruning table is removed, and will instead show-up
  # in the Alteryx reporting tool
  for (i in 1:length(out)) {
    if (grepl("\\sCP\\s", out[i])) {
      strt_table <- i + 1
      break
    }
  }
  model_sum <- length((end_call + 1):(strt_table - 2))
  prune_tbl1 <- out[strt_table:length(out)]
  out <- c(call, out[(end_call + 1):(strt_table - 2)])
  rpart_out <- data.frame(grp = c("Call", rep("Model_Sum", model_sum)), out = out)
  rpart_out$grp <- as.character(rpart_out$grp)
  rpart_out$out <- as.character(out)
  # Pipe delimit the pruning table and then rbind it to the output
  prune_tbl <- NULL
  for (i in 1:length(prune_tbl1)) {
    a_row <- unlist(strsplit(prune_tbl1[i], "\\s"))
    a_row <- a_row[a_row != ""]
    prune_tbl <- c(prune_tbl, paste(a_row[1], a_row[2], a_row[3], a_row[4], a_row[5], a_row[6], sep="|"))
  }
  pt_df <- data.frame(grp = rep("Prune", length(prune_tbl)), out = prune_tbl)
  pt_df$grp <- as.character(pt_df$grp)
  pt_df$out <- as.character(pt_df$out)
  rpart_out <- rbind(rpart_out, pt_df)

  # The leaf summary
  if (is_XDF) {
    print(model_rpart) # Tree Leaf Summary
    leaves <- capture.output(model_rpart)
  } else {
    print(model) # Tree Leaf Summary
    leaves <- capture.output(model)
  }
  leaves_num <- 1:length(leaves)
  start_leaves <- leaves_num[substr(leaves, 1, 5) == "node)"]
  leaves <- leaves[start_leaves:length(leaves)]
  leaves <- gsub(">", "&gt;", leaves)
  leaves <- gsub("<", "&lt;", leaves)
  leaves <- gsub("\\s", "<nbsp/>", leaves) # Order matters
  leaves_df <- data.frame(grp = rep("Leaves", length(leaves)), out = leaves)
  leaves_df$grp <- as.character(leaves_df$grp)
  leaves_df$out <- as.character(leaves_df$out)

  rpart_out <- rbind(rpart_out, leaves_df)

  # Indicate that this is an object of class rpart or rxDTree
  if (is_XDF) {
    rpart_out <- rbind(c("Model_Name", config$model.name), rpart_out, c("Model_Class", "rxDTree"))
  } else {
    rpart_out <- rbind(c("Model_Name", config$model.name), rpart_out, c("Model_Class", "rpart"))
  }

  # Write out the grp-out table for reporting
  # results$out1 <- rpart_out
  rpart_out
}

#' get graphing calls
#'
#' @param config list of config options
#' @param model model object
#' @param is_XDF boolean of whether model is XDF
#' @return params to call AlteryxGraph on
getDTGraphCalls <- function(config, model, is_XDF) {
  # Address the user plot parameters and create the plots

  # The values in the leaf summary
  leaf_sum <- 4
  if (config$do.counts == "True")
    leaf_sum <- 2
  if (model$method != "class")
    leaf_sum <- 0
  print(model$method)

  # Uniform or proportional tree branch lengths
  uniform <- "FALSE"
  fallen <- "TRUE"
  if (config$b.dist == "True") {
    uniform <- "TRUE"
    fallen <- "FALSE"
  }

  # assemble list of function and params to pass to AlteryxGraph
  calls <- list()

  calls$tree <- list()
  calls$tree$f <- "rpart.plot"
  calls$tree$args <- list (
    model_rpart,
    type = 0,
    extra = leaf_sum,
    uniform = uniform,
    fallen.leaves = fallen,
    main = "Tree Plot",
    cex = 1
  )

  calls$prune <- list(
    f = "plotcp",
    args = list(model)
  )
}

#' get component for interactive viz
#'
#' @param model model object
#' @param is_XDF boolean of whether model is XDF
#' @import AlteryxRviz
#' @import htmltools
getDTViz <- function(model, is_XDF) {

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
      dt = renderTree(model, tooltipParams = tooltipParams)
      vimp = varImpPlot(model, height = 300)

      cmat = if (!is.null(model$frame$yval2)){
        iConfusionMatrix(getConfMatrix(model), height = 300)
      }  else {
        tags$div(h1('Confusion Matrix Not Valid'), height = 300)
      }

      k1 = dtDashboard(dt, vimp, cmat)
    }
    # renderInComposer(k1, nOutput = 5)
  }
  k1
}

#' get results in form of output list
#'
#' @param config list of config options
#' @param model model object
#' @param is_XDF boolean of whether model is XDF
getOutputsDT <- function(config, model, is_XDF) {
  # Assemble list to return needed elements to output
  results <- list()

  results$output1 <- getDTPipes(config, model, is_XDF)
  results$output3 <- modelOut(config$model.name, model)

  graph_results <- getDTgraphCalls(config, model, is_XDF)
  results$output2 <- graph_results$tree
  results$output4 <- graph_results$prune

  results$output5 <- getDTViz(model, is_XDF)

  write.Alteryx(results$output1, nOutput = 1)
  write.Alteryx(results$output3, nOutput = 3)

  renderInComposer(results$output5, nOutput = 5)

  results
}

#' process for converting to results list from config and data
#'
#' @param config list of configuration options
#' @param data list of datastream objects
#' @import rpart
#' @import rpart.plot
#' @import AlteryxRhelper
#' @return list of results or results
#' @export
processDT <- function(config, data) {
  # To get run-over-run consistency, set the seed
  set.seed(1)

  config$model.name <- validName(config$model.name)

  checkValidConfig(config, the.data)

  params <- AlteryxPredictive::createDTargs(config, data)
  args <- AlteryxPredictive::argsToDTArgs(params$f, params)
  model <- AlteryxPredictive::doFunction(params$f, args)
  is_XDF <- params$is_XDF

  # post-model error checking & cp adjustment if specified to "Auto"
  model <- adjustCP(config, model)

  getOutputsDT(config, model, is_XDF)

}

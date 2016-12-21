#' Convert data frame into a numeric matrix, filtering out non-numeric columns
#'
#' @param x data frame to coerce to a numeric matrix
df2NumericMatrix <- function(x){
  numNonNumericCols <- NCOL(Filter(Negate(is.numeric), x))
  if (numNonNumericCols == NCOL(x)){
    AlteryxMessage2("All of the provided variables were non-numeric. Please provide at least one numeric variable and try again.", iType = 2, iPriority = 3)
    stop.Alteryx2()
  } else if ((length(numNonNumericCols) > 0) && (numNonNumericCols > 0)){
    AlteryxMessage2("Non-numeric variables were included to glmnet. They are now being removed.", iType = 1, iPriority = 3)
    x <- Filter(is.numeric, x)
  }
  x <- as.matrix(x)
  return(x)
}

#' Figure out which family to use
#'
#' @param inputs input data streams passed to tool
#' @param config configuration passed to tool
#' @return string family
getFamily <- function(inputs, config){
  num_levels <- nlevels(inputs$the.data[,1])

  if (num_levels == 1)
    stop.Alteryx2("Target variable is a factor with only 1 level.")

  family_levels <- c("gaussian", "", "binomial", "multinomial")
  family_levels[min(num_levels, 3) + 1]
}

#' Process Elastic Net Inputs
#'
#' This function takes `inputs` and `config` and returns the model object
#' along with other elements essential to create the reports and plots
#'
#' @param inputs input data streams to the tool
#' @param config configuration passed to the tool
#' @rdname processElasticNet
#' @export
#' @import glmnet
processElasticNet <- function(inputs, config){
  var_names <- getNamesFromOrdered(names(inputs$the.data), config$`Use Weights`)
  glmFun <- if (config$internal_cv) glmnet::cv.glmnet else glmnet::glmnet
  x <- df2NumericMatrix(inputs$the.data[,var_names$x])
  family <- getFamily(inputs, config)
  funParams <- list(x = x,
                    y = inputs$the.data[,var_names$y], family = family,
                    intercept  = !(config$`Omit Constant`), standardize = config$standardize_pred, alpha = config$alpha,
                    weights = if (!is.null(var_names$w)) inputs$the.data[,var_names$w] else NULL,
                    nfolds = if (config$internal_cv) config$nfolds else NULL
  )
  #Set the seed for reproducibility (if the user chose to do so) in the internal-cv case
  if ((config$internal_cv) && (config$set_seed_internal_cv)) {
    set.seed(config$seed_internal_cv)
  }
  the.model <- do.call(glmFun, Filter(Negate(is.null), funParams))
  if (config$internal_cv) {
    #The predict function used with objects of class cv.glmnet can be
    #called with s = "lambda.1se" or s = "lambda.min" .
    if (config$lambda_1se) {
      the.model$lambda_pred <- "lambda.1se"
    } else {
      the.model$lambda_pred <- "lambda.min"
    }
  } else {
    #When the predict function is called with glmnet objects, it either
    #needs a specific value of lambda, or must be called with s= NULL,
    #in which case the predictions will be made at every lambda value in the sequence.
    the.model$lambda_pred <- config$lambda_no_cv
  }
  #Since glmnet and cv.glmnet don't produce a formula, we'll need to save the names
  #of the predictor variables in order to use getXvars downstream, which is required by
  #scoreModel.
  the.model$xvars <- colnames(x)
  the.model$yvar <- var_names$y
  return(the.model)
}

#' Create a data frame with elnet/cv.glmnet containing an elnet model object summary
#'
#'
#' The function createReportGLMNET creates a data frame of an elnet/cv.glmnet model's summary
#' output that can more easily be handled by Alteryx's reporting tools. The
#' function returns a data frame containing the model's coeffcients.
#'
#' @param glmnet_obj glmnet or cv.glmnet model object whose non-zero coefficients are
#'  put into a data frame
#' @author Bridget Toomey
#' @export
#' @family Alteryx.Report
createReportGLMNET <- function(glmnet_obj) {
  coefs_out <- coef(glmnet_obj, s = glmnet_obj$lambda_pred, exact = FALSE)
  #Coerce this result to a vector so we can put it in a data.frame
  #along with the variable names.
  vector_coefs_out <- as.vector(coefs_out)
  return(data.frame(Coefficients = rownames(coefs_out), Values = vector_coefs_out))
}

#' Convert data frame into a numeric matrix, filtering out non-numeric columns
#' and warning the user (by displaying a message passed in as an argument) when
#' such filtering occurs.
#'
#' @param x data frame to coerce to a numeric matrix
#' @param filtering_message a message telling the end user that the input data
#'   frame will have its non-numeric columns removed
#' @param convertVectorToDataFrame a boolean specifying whether x should be
#' converted to a df if it's provided as a vector.
#' @export
df2NumericMatrix <- function(
  x,
  filtering_message,
  convertVectorToDataFrame = FALSE
){
  if(
    is.vector(x) &&
    convertVectorToDataFrame
  ){
    x <- as.data.frame(x)
  }
  if(!inherits(x, "data.frame")){
    stop.Alteryx2(
      paste0(
        "An object not inheriting from class data.frame was passed to df2NumericMatrix.",
        "Please contact Alteryx Support. "
      )
    )
  }
  numNonNumericCols <- NCOL(Filter(Negate(is.numeric), x))
  if (numNonNumericCols == NCOL(x)){
    AlteryxMessage2(
      paste0(
        "All of the provided variables were non-numeric. ",
        "Please provide at least one numeric variable and try again."
      ),
      iType = 2,
      iPriority = 3
    )
    stop.Alteryx2()
  } else if ((length(numNonNumericCols) > 0) && (numNonNumericCols > 0)){
    AlteryxMessage2(
      filtering_message,
      iType = 1,
      iPriority = 3
    )
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
  x <- df2NumericMatrix(
    x = inputs$the.data[,var_names$x, drop = FALSE],
    filtering_message = "Non-numeric variables are among the predictors. They are now being removed.",
    convertVectorToDataFrame = TRUE
  )
  if (ncol(x) < 2) {
    stop.Alteryx2(
      paste0(
        "Regularization requires at least two numeric predictors. ",
        "Please switch to a non-regularized model, or use more numeric predictors."
      )
    )
  }
  glmFun <- if (config$internal_cv) glmnet::cv.glmnet else glmnet::glmnet
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
  if (family != "gaussian") {
    the.model$ylevels <- levels(inputs$the.data[,var_names$y])
  }
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

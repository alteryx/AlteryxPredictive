#' Get names list for x, y, and weights
#'
#' @param use_weights boolean for whether weights are in data
#' @param data_names vector of names for data
#' @return list of x, y, and weights with respective names
#' @export
getNamesFromOrdered <- function(use_weights, data_names) {
  name_y_var <- data_names[1]
  if(use_weights){
    names_x_var <- data_names[2:length(data_names)-1]
    name_weights_var <- data_names[length(data_names)]
  } else {
    names_x_var <- data_names[2:length(data_names)]
    name_weights_var <- NULL
  }
  # Make sure that the target variable is not included in the set of
  # predictor variables. If it is, then remove it from the set.
  if (name_y_var %in% names_x_var) {
    names_x_var <- names_x_var[names_x_var != name_y_var]
  }
  list(x = names_x_var, y = name_y_var, w = name_weights_var)
}

#' Create formula string
#'
#' @param x_vars vector of strings of x variable field names
#' @param y_var string of y variable field name
#' @return formula of y_var ~ x_vars
makeFormula <- function(x_vars, y_var) {
  x_string <- paste(x_vars, collapse = " + ")
  as.formula(paste0(y_var, " ~ ", x_string))
}

#' Execute function on parameters
#' To-Do: re-specify call to be appropriate
#'
#' @param f function string
#' @param param_list list of parameters to pass to f
#' @return model object
#' @export
doFunction <- function(f, param_list) {
  do.call(match.fun(f), param_list)
}

#' Prepare model for output
#'
#' @param model_name string - name of model to be output
#' @param model_obj model object to output
#' @return key value pair list with name and model
#' @export
modelOut <- function(model_name, model_obj) {
  out_list <- vector(mode = "list", length = 2)
  out_list[[1]] <- c(model_name)
  out_list[[2]] <- list(model_obj)
  names(out_list) <- c("Name", "Object")
  out_list
}

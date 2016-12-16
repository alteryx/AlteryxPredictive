#' getYVar function
#'
#' @param mod model object
#' @export
#' @author Ramnath Vaidyanathan, Bridget Toomey
#' @rdname getYVar
getYVar <- function(mod) {
  UseMethod('getYVar')
}

# Get y variable from model object
#' @export
#' @rdname scoreModel
getYVar.default <- function(mod){
  as.character(formula(mod))[2]
}


#' @export
#' @rdname scoreModel
getYVar.elnet <- function(mod) {
  mod$yvar
}

#Note that this will need to be updated once regularized logistic regression
#is finished. We can put an if statement to check if mod$glmnet.fit is elnet
#or lognet and proceed accordingly.
#' @export
#' @rdname scoreModel
getYVar.cv.glmnet <- function(mod) {
  mod$yvar
}

#' @export
#' @rdname scoreModel
getYVar.naiveBayes <- function(mod) {
  mod$yvars
}

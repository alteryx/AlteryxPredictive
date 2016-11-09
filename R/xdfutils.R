#' extract levels from xdf data
#'
#' @param formula R formula
#' @param xdf path to xdf file
#' @return names of levels - vector
#' @export
getXdfLevels <- function(formula, xdf) {
  factors <- RevoScaleR::rxSummary(formula, data = xdf)$categorical
  factor_names <- sapply(factors, function(x) names(x)[1])
  if (length(factor_names) == 1) {
    the_levels <- list(factor_names = as.character(factors[[1]][[1]]))
  } else {
    the_levels <- sapply(factors, function(x) as.character(x[[1]]))
    names(the_levels) <- factor_names
  }
  the_levels
}

#' extract xdf properties from incoming xdf data
#'
#' @param input_name name of incoming connection
#' @param default default xdf settings for mocking in the R console
#' @return list with boolean is_XDF and string xdf_path representing
#'  whether data is XDF and the path of the data
#' @import rjson
#' @export
getXdfProperties <- function(input_name, default = NULL) {
  if(!inAlteryx()){
    default
  } else {
    is_XDF <- FALSE
    meta_data <- AlteryxRDataX::read.AlteryxMetaInfo(input_name)
    the_source <- as.character(meta_data$Source)
    if (all(substr(the_source, 3, 9) == "Context")) {
      context_list <- rjson::fromJSON(the_source[1])
      if (context_list$Context == "XDF") {
        is_XDF <- TRUE
        xdf_path <- context_list$File.Loc
      } else {
        stop.Alteryx2("At this time only XDF scaling is supported.")
      }
    } else {
      xdf_path = ""
    }
    list(is_XDF = is_XDF, xdf_path = xdf_path)
  }
}

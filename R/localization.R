#' localization.R
#
################################################################################
# This file contains all of the R functions necessary to localize Analytics
# Products R code.
################################################################################

#' @param stringIn the input string to translate
#' @export
XMSG <- function(stringIn){
  return(paste0('[[[', toupper(stringIn), ']]]'))
}

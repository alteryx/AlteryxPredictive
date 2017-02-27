#' localization.R
#
################################################################################
# This file contains all of the R functions necessary to localize Analytics
# Products R code.
################################################################################

#' @param in.translString_sc the input string to translate
#' @param in.firstBindVar_sc the first bind variable
#' @param in.secondBindVar_sc the second bind variable
#' @param in.thirdBindVar_sc the third bind variable
#' @param in.fourthBindVar_sc the fourth bind variable
#' @param in.fifthBindVar_sc the fifth bind variable
#' @param in.sixthBindVar_sc the sixth bind variable
#' @export
XMSG <- function(
  in.targetString_sc,
  in.firstBindVariable_sc = NULL,
  in.secondBindVariable_sc = NULL,
  in.thirdBindVariable_sc = NULL,
  in.fourthBindVariable_sc = NULL,
  in.fifthBindVariable_sc = NULL,
  in.sixthBindVariable_sc = NULL
){
  bindVariable_vc <- c(
    in.firstBindVariable_sc,
    in.secondBindVariable_sc,
    in.thirdBindVariable_sc,
    in.fourthBindVariable_sc,
    in.fifthBindVariable_sc,
    in.sixthBindVariable_sc
  )
  returnValue_sc <- paste0(
    '[[[',
    in.targetString_sc
  )
  if(length(bindVariable_vc) > 0){
    for(i in 1:length(bindVariable_vc)){
      returnValue_sc <- paste0(
        returnValue_sc,
        '++',
        bindVariable_vc[i]
      )
    }
  }
  returnValue_sc <- paste0(
    returnValue_sc,
    ']]]'
  )
  return(returnValue_sc)
}

xmsg_and_pipe_delimit <- function(in.term_vc){
  returnValue_sc <- paste0(
    unlist(
      sapply(
        X = 1:length(in.term_vc),
        FUN = function(X){
          XMSG(in.term_vc[X])
        }
      )
    ),
    collapse = '|'
  )
  return(returnValue_sc)
}

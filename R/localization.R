#' localization.R
#
################################################################################
# This file contains all of the R functions necessary to localize Analytics
# Products R code.
################################################################################

#' translates input string and embeds any bind-variable values in result
#' @param in.targetString_sc the input string to translate
#' @param in.firstBindVariable_sc the first bind variable
#' @param in.secondBindVariable_sc the second bind variable
#' @param in.thirdBindVariable_sc the third bind variable
#' @param in.fourthBindVariable_sc the fourth bind variable
#' @param in.fifthBindVariable_sc the fifth bind variable
#' @param in.sixthBindVariable_sc the sixth bind variable
#' @return translated string with embedded bind-variable values (if any)
#' @export
XMSG2 <- function(
  in.targetString_sc,
  in.firstBindVariable_sc = NULL,
  in.secondBindVariable_sc = NULL,
  in.thirdBindVariable_sc = NULL,
  in.fourthBindVariable_sc = NULL,
  in.fifthBindVariable_sc = NULL, in.sixthBindVariable_sc = NULL
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


XMSG <- function(...){
  saveRDS(list(...), file="C:/Documents/XMSGargs.rds")
  if(inAlteryx()){
    AlteryxRDataX:::XMSG(...)
  } else {
    XMSG2(...)
  }
}


XMSG3 <- function(
  in.targetString_sc,
  in.firstBindVariable_sc = NULL,
  in.secondBindVariable_sc = NULL,
  in.thirdBindVariable_sc = NULL,
  in.fourthBindVariable_sc = NULL,
  in.fifthBindVariable_sc = NULL,
  in.sixthBindVariable_sc = NULL
){
  if(inAlteryx()){
    AlteryxRDataX:::XMSG(
      in.targetString_sc,
      in.firstBindVariable_sc ,
      in.secondBindVariable_sc,
      in.thirdBindVariable_sc ,
      in.fourthBindVariable_sc,
      in.fifthBindVariable_sc ,
      in.sixthBindVariable_sc 
    )
  } else {
    XMSG2(
      in.targetString_sc,
      in.firstBindVariable_sc ,
      in.secondBindVariable_sc,
      in.thirdBindVariable_sc ,
      in.fourthBindVariable_sc,
      in.fifthBindVariable_sc ,
      in.sixthBindVariable_sc 
    )
  }
}

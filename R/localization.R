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
  in.translString_sc,
  in.firstBindVar_sc = NULL,
  in.secondBindVar_sc = NULL,
  in.thirdBindVar_sc = NULL,
  in.fourthBindVar_sc = NULL,
  in.fifthBindVar_sc = NULL,
  in.sixthBindVar_sc = NULL
){
  return(
    paste0(
      'main string: [[[',
      toupper(
        in.translString_sc
        ),
      ']]]. bindvars: ',
      in.firstBindVar_sc,
      " ",
      in.secondBindVar_sc,
      " ",
      in.thirdBindVar_sc,
      " ",
      in.fourthBindVar_sc,
      " ",
      in.fifthBindVar_sc,
      " ",
      in.sixthBindVar_sc
      )
    )
}



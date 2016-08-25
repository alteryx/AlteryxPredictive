#' Returns TRUE if called inside an Alteryx R Tool.
#'
#' @export
inAlteryx <- function(){
  exists("AlteryxDataOutput", .GlobalEnv)
}

#' Stop
#'
#' @param msg message
#' @param ... extra arguments to pass on to stop.Alteryx
#' @export
stop.Alteryx2 <- function(msg, ...){
  if (inAlteryx()){
    AlteryxRDataX::stop.Alteryx(msg, ...)
  } else {
    stop(msg)
  }
}

#' Message
#'
#' @param msg message
#' @param ... extra arguments to pass on to AlteryxMessage
#' @export
AlteryxMessage2 <- function(msg, ...){
  if (inAlteryx()) {
    AlteryxRDataX::AlteryxMessage(msg, ...)
  } else {
    message(msg, ...)
  }
}

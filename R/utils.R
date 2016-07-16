#' Returns TRUE if called inside an Alteryx R Tool.
#'
#' @export
inAlteryx <- function(){
  'package:AlteryxRDataX' %in% search()
}

#' Stop
#'
#' @param msg message
#' @param ... extra arguments to pass on to stop.Alteryx
#' @export
stop.Alteryx <- function(msg, ...){
  if (inAlteryx()){
    AlteryxRDataX::stop.Alteryx(msg, ...)
  } else {
    stop(msg)
  }
}

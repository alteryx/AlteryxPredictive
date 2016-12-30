#' Create a dashboard outputting a single message
#'
#' @param msg message to output
#' @return dashboard object for rendering
badDash <- function(msg) {
  tags$div(tags$h4(msg))
}

#' Create a dashboard outputting a single message
#'
#' @param msg message to output
#' @return dashboard object for rendering
#' @author Todd Morley
badDash <- function(
  messageIn,
  titleIn = "Error",
  colorIn = 'aqua',
  widthIn = 12
){
  requireNamespace("flightdeck")
  header <- fdHeader(
    title = titleIn,
    titleWidth = 600)
  row <- fdRow(
    fdBox(
      fdInfoBox(
        title = NULL,
        value = messageIn,
        subtitle = NULL,
        icon = fdIcon('flash', 'entypo'),
        color = colorIn,
        width = widthIn
      ),
      width = widthIn
    )
  )
  page <- fdPage(
    row,
    id = 'page',
    display = TRUE
  )
  body <- fdBody(
    page
  )
  sidebar <- fdSidebar()
  fdBoard(
    header = header,
    sidebar = NULL,
    body = body,
    fixed = TRUE
  )
}

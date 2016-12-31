#' Create a dashboard outputting a single message
#'
#' @param messageIn message to output
#' @param titleIn title of dash
#' @param colorIn color in dash
#' @param widthIn width
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

#' Helpful wrapper around fitted and actual values for generating confusion matrix
#'
#' @param fitted_values fitted values
#' @param actual_values actual values
#' @return confusion matrix
getBinaryConfusionMatrix <- function(
  fitted_values,
  actual_values
){
  if(is.factor(fitted_values)){
    fitted_values <- as.numeric(fitted_values) - 1
  }
  if(is.factor(actual_values)){
    actual_values <- as.numeric(actual_values) - 1
  }
  true_positive_count <- length(
    intersect(
      which(fitted_values == 1),
      which(actual_values == 1)
    )
  )
  false_positive_count <- length(which(fitted_values > actual_values))
  true_negative_count <- length(
    intersect(
      which(fitted_values == 0),
      which(actual_values == 0)
    )
  )
  false_negative_count <- length(which(fitted_values < actual_values))
  confusion_matrix_m <- matrix(
    data = c(
      true_positive_count,
      false_positive_count,
      false_negative_count,
      true_negative_count
    ),
    nrow = 2,
    ncol = 2
  )
  rownames(confusion_matrix_m) <- c('Predicted Positive', 'Predicted Negative')
  colnames(confusion_matrix_m) <- c('Actual Positive', 'Actual Negative')

  confusion_matrix_m
}

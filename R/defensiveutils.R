#' Check if data contains missing values, omit those records
#'
#' @param data dataframe to check
#' @return data with missing records excluded
checkMissing.omit <- function(data) {
  if (sum(complete.cases(data)) < NROW(data)) {
    AlteryxMessage2("The data contains missing values. Rows with missing data are being removed.", iType = 1, iPriority = 3)
    data <- (data)[complete.cases(data),]
    if (NROW(data) == 0) {
      stop.Alteryx2("Every row had at least one missing value. Clean your data and try again.")
    }
  }
}

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
  data
}

#' Check if data has enough rows
#'
#' @param data dataframe
#' @param threshold minimum number of records needed to not give warning
#' @param mult multiplier portion of data used for model training
#' @param msg output message (warning)
#' @return no return - only throws warning
#' @export
checkLowN <- function(data,
                      threshold = 25,
                      mult = 1,
                      msg
){
  if(NROW(data)*mult < threshold){
    AlteryxMessage2(msg, 2, 2)
  }
}

#' Check if positive class is present in vector
#' Will return same value if in vector, value from data if level is in vector
#' with different case / trim, and an empty string if no similar value is present
#'
#' @param v vector to check for presence of class
#' @param lvl level of class to check for presence of
#' @return positive class
verifyClass <- function(v, lvl){
  if(is.null(lvl) || is.na(lvl) || lvl == ""){
    return(lvl)
  }
  v_levels <- levels(v)
  if(is.null(v_levels)){
    # numeric v
    return ("")
  } else if(length(v_levels) == 1){
    AlteryxMessage2(
      paste(
        "All records have the same results as a target variable.",
        "Modeling will not be helpful"
      ),
      2,
      2 #### Warning
    )
    return(v_levels[[1]])
  } else {
    ### Non-trivial cases - 2 or more levels
    if(lvl %in% v_levels){
      return(lvl)
    } else {
      matches <- which(tolower(trimws(v_levels)) == tolower(trimws(lvl)))
      if(length(matches) == 1) {
        AlteryxMessage2(
          paste(
            "The provided positive class was coerced by",
            "trimming and/or case-change to match the target variable."
          ),
          2,
          2 #### Warning
        )
        return(v_levels[matches])
      } else {
        AlteryxMessage2(
          "The provided positive class was not present in target variable.",
          2,
          2 #### Warning
        )
        return ("")
      }
    }
  }
}

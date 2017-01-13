#' Translates an R squared into an adjusted R squared, returning NaN
#' if the input is Nan.
#'
#' @param r_squared R squared
#' @param n number of records (rows)
#' @param p number of independent variables (predictors)
#' @param intercept_degrees_freedom one if model includes intercept,
#'   else zero
#'
#' @export
adj_r_squared <- function(
  r_squared,
  n,
  p,
  intercept_degrees_freedom
){
  if(is.nan(x = r_squared)){
    return(NaN)
  }
  return(
    1 -
    (1 - r_squared) *
    (n - intercept_degrees_freedom) /
    (n - p - intercept_degrees_freedom)
  )
}

#' Tries very hard to compute a sensible R squared, else returns NaN.
#'
#' @param numeric_vector_1 one numeric vector
#' @param numeric_vector_2 another numeric vector of the same length
#'
#' @export
rSquared <- function(
  numeric_vector_1,
  numeric_vector_2
){
  if(
    !inherits(x = numeric_vector_1, what = 'numeric') ||
    !inherits(x = numeric_vector_2, what = 'numeric')
  ){
    stop.Alteryx2(
      msg = paste(
        "An object other than a numeric vector was passed to",
        "AlteryxPredictive::rSquared().  Please contact Alteryx Support. "
      )
    )
  }
  if(length(numeric_vector_1) != length(numeric_vector_2)){
    stop.Alteryx2(
      msg = paste(
        "The vectors passed to AlteryxPredictive::rSquared() were of",
        "unequal length.  Please contact Alteryx Support. "
      )
    )
  }
  r_squared <- NULL
  try(
    expr = r_squared <-
      cov(numeric_vector_1, numeric_vector_2)^2 /
      (var(numeric_vector_1) * var(numeric_vector_2)),
    silent = TRUE
  )
  if(
    is.null(r_squared) ||
    is.nan(r_squared) ||
    r_squared < 0.0 ||
    r_squared > 1.0
  ){
    try(
      expr = r_squared <-
        exp(
          2 * log(cov(numeric_vector_1, numeric_vector_2)) -
          log(var(numeric_vector_1)) -
          log(var(numeric_vector_2))
        ),
      silent = TRUE
    )
  }
  if(
    is.null(r_squared) ||
    is.nan(r_squared) ||
    r_squared < 0.0 ||
    r_squared > 1.0
  ){
    r_squared <- NaN
  }
  return(r_squared)
}

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
    message(msg)
  }
}

#' Utility function to check to see if the necessary packages are
#' installed and install them if they're not.
#'
#'
#' @param packages vector of package names to check and install.
#' @export
#' @author Bridget Toomey, Dan Putler
checkInstalls <- function(packages) {
  # See if the desired packages are installed, and install if they're not
  if (!all(packages %in% row.names(installed.packages()))) {
    # Use the IE based "Internet2" since it is most reliable for this action,
    # it will be switched back at the end
    setInternet2(use = TRUE)
    # Make sure the path to the users library is in place and create it if it
    # is not
    minor_ver <- strsplit(R.Version()$minor, "\\.")[[1]][1]
    R_ver <- paste(R.Version()$major, minor_ver, sep = ".")
    the_path <- paste0(normalizePath("~"), "\\R\\win-library\\", R_ver)
    # Create the user's personal folder if it doesn't already exist
    if (!dir.exists(the_path)) {
      dir.create(the_path, recursive = TRUE, showWarnings = FALSE)
    }
    # The set of possible repositories to use
    repos <- c("http://cran.revolutionanalytics.com", "https://cran.rstudio.com")
    # Select a particular repository
    repo <- sample(repos, 1)
    missingPackages <- packages[which(!(packages %in% row.names(installed.packages())))]
    install.packages(missingPackages, lib = the_path, repos = repo)
    setInternet2(use = FALSE)
  }
}

# Create an empty plot.
emptyPlot <- function(){
  plot(x = c(0,1), y = c(0,1), type = "n",
    xlab = "", ylab = "", xaxt = "n", yaxt = "n"
  )
}

computeWidth <- function(title, cex = 1.5, dpi = 96){
  strwidth(title, cex = cex, units = 'in')*dpi
}

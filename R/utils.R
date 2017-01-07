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

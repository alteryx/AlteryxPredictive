#' Serialize an arbitrary R object into an ASCII string
#'
#' This function converts an arbitrary R object and serializes it into an ASCII
#' string.  Spatial objects become Well Known Text
#'
#' @param the.object object to serialize
#' @author Rob Bryan
serializeObject <- function(the.object){
  if (class(the.object) == "data.frame" && length(the.object) == 2 && the.object[[1]] == "POINT EMPTY" && the.object[[2]] == "SpatialIsNull"){
    return (as.character(the.object[[1]]))
  } else {
    return(rawToChar(serialize(the.object, connection = NULL, ascii = TRUE)))
  }
}


#' Unserialize a string produced by serializeObject
#'
#'
#' The function unserializeObject takes a string produced by serializeObject
#' and unserializes it
#'
#' @param the.string string to unserialize
#' @author Rob Bryan
unserializeObject <- function(the.string){
  if (identical(substr(the.string,1,2),"A\n")){
    return(unserialize(charToRaw(as.character(the.string))))
  }
}

# Check if an object is a model object created by serializeObject
# @author Ramnath Vaidyanathan
checkModelObjects <- function(name, default){
  meta.data <- if (inAlteryx()){
    AlteryxRDataX::read.AlteryxMetaInfo(name)
  } else {
    data.frame(Name = names(default))
  }
  names.field <- as.character(meta.data$Name)
  if(identical(names.field, c("Name", "Object"))) {
    invisible()
  } else {
    stop.Alteryx2("The input stream does not appear to be an R model object.")
  }
}


#' Read an Alteryx stream consisting of a table of model objects
#'
#' @param name name of the Alteryx input to read.
#' @param default default to be used when run outside of an Alteryx workflow.
#' @export
readModelObjects <- function(name, default){
  checkModelObjects(name, default)
  models <- if (inAlteryx()) AlteryxRDataX::read.Alteryx(name) else default
  m <- lapply(models$Object, function(x){
    unserializeObject(as.character(x))
  })
  setNames(m, models$Name)
}

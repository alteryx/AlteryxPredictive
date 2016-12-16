#' Perform a nested test on linear models
#'
#' This function performs a nested test for linear and generalized linear models
#' @author Dan Putler, Ramnath Vaidyanathan
#' @param inputs list of objects consisting of 1 data frame and 2 model objects.
#' @export
#' @examples
#' mod1 <- lm(mpg ~ ., data = mtcars)
#' mod2 <- lm(mpg ~ wt, data = mtcars)
#' inputs <- list(
#'   list(Name = 'mod1', Object = mod1),
#'   list(Name = 'mod2', Object = mod2),
#'   mtcars
#' )
#' performNestedTest(inputs)
performNestedTest <- function(inputs){
  modelObjects <- Filter(hasModel, inputs)
  the.data <- Filter(Negate(hasModel), inputs)[[1]]
  models <- lapply(modelObjects, '[[', 'Object')
  plyr::l_ply(.data = models, .fun = checkModelType)
  modelNames <- as.character(sapply(modelObjects, '[[', 'Name'))
  modelClasses <- sapply(models, function(d){class(d)[1]})
  yVars <- sapply(models, getYVar)
  xVars <- lapply(models, getXVars)


  # Some initial error checking
  if(length(unique(yVars)) != 1) {
    stop.Alteryx2("The models have different target variables")
  }
  if(modelClasses[1] != modelClasses[2]) {
    stop.Alteryx2("The models are not of the same class")
  }


  sm_i <- if (length(xVars[[1]]) < length(xVars[[2]])) 1 else 2
  smList <- xVars[sm_i][[1]]; lgList <- xVars[-sm_i][[1]]
  smModel <- models[sm_i][[1]]; lgModel <- models[-sm_i][[1]]
  smName <- modelNames[sm_i]; lgName <- modelNames[-sm_i]

  if(!all(smList %in% lgList)) {
    stop("The smaller model is not nested in the larger model")
  }

  # Some additional error checking
  if(!all(c(yVars[1], lgList) %in% names(the.data))) {
    stop("Not all the variables used in the models are in the data stream")
  }

  the_anova <- anova(smModel, lgModel)

  test_title <- getTestTitle(smList, lgList, lgName)
  ## Format for reporting


  # The nature of the test (F versus chi-square) depends on whether the models
  # are lm or glm models. The code below provides the right labels for the test.
  # In addition, the p-value is obtained for a lm class model and calculated for
  # a glm model.
  if(modelClasses[1] == "lm") {
    col_names <- c("DF", "Sum of Squares", "F", "Pr(>F)", "  ")
    p_val <- the_anova[[6]][2]
  } else {
    col_names <- c("DF", "Chi-Sq", "Pr(>Chi-Sq)", "  ")
    p_val <- 1 - pchisq(the_anova[[4]][2], the_anova[[3]][2])
  }

  # Given the p-value, create the "star" significance indicator
  the_stars <- getStars(p_val)

  # The p-value needs to be reformatted to match normal R reporting.
  p_txt <- getPvalTxt(p_val)

  # Gather together the statistics and print the summary to the output window
  if(modelClasses[1] == "lm") {
    the_stats <- paste(the_anova[[3]][2], round(the_anova[[4]][2], 2),
      round(the_anova[[5]][2], 4), p_txt, the_stars, sep = "|"
    )
    out_df <- data.frame(DF = the_anova[[3]][2], SS = round(the_anova[[4]][2], 2),
      Fstat = round(the_anova[[5]][2], 4), p_val = p_txt,
      the_stars = the_stars
    )
  } else {
    the_stats <- paste(the_anova[[3]][2], round(the_anova[[4]][2], 4),
      p_txt, the_stars, sep = "|")
    out_df <- data.frame(DF = the_anova[[3]][2], Dev = round(the_anova[[4]][2], 4),
      p_txt, the_stars = the_stars
    )
  }
  names(out_df) <- col_names
  cat(paste(test_title, "\n", sep=""))
  print(out_df)

  # Create the key-value pair table and write it out
  key_value <- data.frame(
    grp = c("Title", "Table"),
    out = c(test_title, the_stats),
    stringsAsFactors = FALSE
  )
  key_value <- rbind(key_value, c('Model_Class', modelClasses[1]))
  return(key_value)
}

#' Unserialize a list of inputs some of which might contain serialized model objects
#'
#' @author Dan Putler, Ramnath Vaidyanathan
#' @param inputs list of inputs
#' @export
unserializeInputs <- function(inputs){
  lapply(inputs, function(input){
    if (hasModel(input)){
      list(
        Name = as.character(input$Name),
        Object = unserializeObject(as.character(input$Object))
      )
    } else {
      input
    }
  })
}

#Check to see if the model is an appropriate type for the Nested Test tool
checkModelType <- function(model){
  if (inherits(model, "glmnet") || inherits(model, "cv.glmnet")) {
    stop.Alteryx2("Regularized models are not supported in the Nested Test tool at this time. Use a non-regularized model and try again.")
  }
}


# Get significance stars from p value
getStars <- function(p_val){
  as.character(cut(p_val,
    c(0, 0.001, 0.01, 0.05, 0.1, 1),
    c('***', '**', '*', '.', ''),
    right = FALSE
  ))
}

# Get p value text from p value by rounding
getPvalTxt <- function(p_val){
  p_txt <- as.character(p_val)
  p_txt[p_val < 2.2e-16] <- "< 2.2e-16"
  p_txt[p_val >= 0.000005] <- round(p_val, 5)
  return(p_txt)
}

getTestTitle <- function(smList, lgList, lgName){
  # The variables removed from the large model to form the small model
  removed_vars <- lgList[!(lgList %in% smList)]
  # Create a descriptive title for the test
  if(length(removed_vars) == 1) {
    test_title <- paste(
      "The Effect of Removing the Variable", removed_vars, "from", lgName
    )
  } else {
    rm_vars <- paste(removed_vars[1:(length(removed_vars) - 1)], collapse = ", ")
    rm_vars <- paste(rm_vars, "and", removed_vars[length(removed_vars)])
    test_title <- paste(
      "The Effect of Removing the Variables", rm_vars, "from", lgName
    )
  }
}

# Check if an input has a model object
hasModel <- function(x){
  identical(names(x), c("Name", "Object"))
}

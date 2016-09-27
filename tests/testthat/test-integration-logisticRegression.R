context("Logistic Regression: helper functions testing ")

config <- list(
  `graph.resolution` = '1x',
  `the.link` = 'logit',
  `model.name` = 'Basic_Logistic_Regression',
  `used.weights` = FALSE,
  `X Vars` = c('gre', 'gpa', 'rank'),
  `Y Var` = 'admit'
)

##----

#' #### Read Inputs
#'
#' This is a named list of all inputs that stream into the R tool.
#' We also specify defaults for use when R code is run outside Alteryx.
inputs <- list(
  the.data = admission[, c(config$`Y Var`, config$`X Vars`)],
  XDFInfo = list(is_XDF = FALSE, xdf_path = NULL)
)


#' ### Run and Create Outputs
exp_logistic_model <- glm(
  admit ~ gre + gpa + rank, data = admission,
  family = binomial(logit)
)

test_that("admission data with logit link returns correct coefficients", {
  result <- AlteryxPredictive:::runLogisticRegression(inputs, config)
  expect_equal(
    result$Object[[1]]$coefficients,
    exp_logistic_model$coefficients
  )
})

test_that("admission data with probit link returns correct coefficients", {
  config$the.link <- "probit"
  exp_probit_model <- update(exp_logistic_model, family = binomial(probit))
  result <- AlteryxPredictive:::runLogisticRegression(inputs, config)
  expect_equal(
    result$Object[[1]]$coefficients,
    exp_probit_model$coefficients
  )
})

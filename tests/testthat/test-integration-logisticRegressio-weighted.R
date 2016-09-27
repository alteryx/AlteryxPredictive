context("Logistic Regression: weighted version with api data")

config <- list(
  `graph.resolution` = '1x',
  `the.link` = 'logit',
  `model.name` = 'Basic_Logistic_Regression',
  `used.weights` = TRUE,
  `X Vars` = c('ell', 'meals'),
  `W Var` = 'pw',
  `Y Var` = 'sch.wide'
)

##----

#' #### Read Inputs
#'
#' This is a named list of all inputs that stream into the R tool.
#' We also specify defaults for use when R code is run outside Alteryx.

data(api, package = "survey")
inputs <- list(
  the.data = apiclus1[, c(config$`Y Var`, config$`X Vars`, config$`W Var`)],
  XDFInfo = list(is_XDF = FALSE, xdf_path = NULL)
)


#' ### Run and Create Outputs
design <- survey::svydesign(id = ~1, weights = ~pw, data = apiclus1)
exp_model <- survey::svyglm(sch.wide ~ ell + meals , design = design, family=quasibinomial(logit))

test_that("admission data with probit link", {
  result <- AlteryxPredictive:::runLogisticRegression(inputs, config)
  expect_equal(result$Object[[1]]$coefficients,
               exp_model$coefficients)
})

context("Logistic Regression: basic version with admission data")

config <- list(
  `graph.resolution` = '1x',
  `Link` = 'logit',
  `Model Name` = 'Basic_Logistic_Regression',
  `Use Weights` = FALSE,
  `Weight Vec` = NULL,
  `X Vars` = c('gre', 'gpa', 'rank'),
  `Y Var` = 'admit'
)

inputs <- list(
  the.data = AlteryxPredictive::admission[, c(config$`Y Var`, config$`X Vars`)],
  XDFInfo = list(is_XDF = FALSE, xdf_path = NULL)
)

testDir = '~/Desktop/SNIPPETS/dev/Predictive_Refresh/Logistic_Regression/Extras/Tests'
# AlteryxRhelper::makeWorkflow(
#   template = file.path(testDir, "SampleTest.yxmd"),
#   data = inputs$the.data,
#   config = config,
#   inputs_id = 2,
#   config_id = 8,
#   outFile = file.path(testDir, "LogisticTest1.yxmd")
# )


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
  config$Link <- "probit"
  exp_probit_model <- update(exp_logistic_model, family = binomial(probit))
  result <- AlteryxPredictive:::runLogisticRegression(inputs, config)
  expect_equal(
    result$Object[[1]]$coefficients,
    exp_probit_model$coefficients
  )
})

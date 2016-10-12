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


#' ### Run and Create Outputs
exp_logistic_model <- glm(
  admit ~ gre + gpa + rank, data = admission,
  family = binomial(logit)
)
coefs <- coef(exp_logistic_model)
coef_dframe <- data.frame(
  Variable = names(coefs),
  Coefficient = format(coefs, digits = 5)
)


test_that("admission data with logit link returns correct coefficients", {
  result <- AlteryxPredictive:::runLogisticRegression(inputs, config)
  expect_equal(
    result$Object[[1]]$coefficients,
    exp_logistic_model$coefficients
  )
})


testDir = '~/Desktop/SNIPPETS/dev/Predictive_Refresh/Logistic_Regression/Extras/Tests'
comment = 'This workflow tests that admission data with logit link returns correct coefficients'
# AlteryxRhelper::makeWorkflow2(
#   template = file.path(testDir, "SampleTest.yxmd"),
#   repl = list(
#     list(node = 2, data = inputs$the.data, type = 'input'),
#     list(node = 8, data = config, type = 'config'),
#     list(node = 15, data = coef_dframe, type = 'input'),
#     list(node = 17, data = comment, type = 'text'),
#     list(node = 16, data = 'Logistic Regression Test', type = 'text')
#   ),
#   outFile = file.path(testDir, "LogisticTest1.yxmd")
# )

exp_probit_model <- update(exp_logistic_model, family = binomial(probit))
config$Link <- "probit"
test_that("admission data with probit link returns correct coefficients", {
  result <- AlteryxPredictive:::runLogisticRegression(inputs, config)
  expect_equal(
    result$Object[[1]]$coefficients,
    exp_probit_model$coefficients
  )
})

coefs <- coef(exp_probit_model)
coef_dframe <- data.frame(
  Variable = names(coefs),
  Coefficient = format(coefs, digits = 5)
)

comment = 'This workflow tests that admission data with probit link returns correct coefficients'
# AlteryxRhelper::makeWorkflow2(
#   template = file.path(testDir, "SampleTest.yxmd"),
#   repl = list(
#     list(node = 2, data = inputs$the.data, type = 'input'),
#     list(node = 8, data = config, type = 'config'),
#     list(node = 15, data = coef_dframe, type = 'input'),
#     list(node = 17, data = comment, type = 'text'),
#     list(node = 16, data = 'Logistic Regression Test', type = 'text')
#   ),
#   outFile = file.path(testDir, "LogisticTest2.yxmd")
# )

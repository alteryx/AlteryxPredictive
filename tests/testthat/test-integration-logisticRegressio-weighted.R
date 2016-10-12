context("Logistic Regression: weighted version with api data")

config <- list(
  `graph.resolution` = '1x',
  `Link` = 'logit',
  `Model Name` = 'Logistic_Regression_With_Weights',
  `Use Weights` = TRUE,
  `X Vars` = c('ell', 'meals'),
  `Weight Vec` = 'pw',
  `Y Var` = 'sch.wide'
)

data(api, package = "survey")
inputs <- list(
  the.data = apiclus1[, c(config$`Y Var`, config$`X Vars`, config$`Weight Vec`)],
  XDFInfo = list(is_XDF = FALSE, xdf_path = NULL)
)



#' ### Run and Create Outputs
library(survey)
design <- svydesign(id = ~1, weights = ~pw, data = apiclus1)
exp_model <- svyglm(sch.wide ~ ell + meals , design = design,
  family = quasibinomial(logit)
)

test_that("admission data with weights and logit link", {
  skip("this test gives a false positive when run using devtools::test()")
  result <- AlteryxPredictive:::runLogisticRegression(inputs, config)
  expect_equal(result$Object[[1]]$coefficients, exp_model$coefficients)
})


coefs <- coef(exp_model)
coef_dframe <- data.frame(
  Variable = names(coefs),
  Coefficient = format(coefs, digits = 5)
)

testDir = '~/Desktop/SNIPPETS/dev/Predictive_Refresh/Logistic_Regression/Extras/Tests'
comment = 'This workflow tests that apiclus data with weights returns corrects coefficients.'
# AlteryxRhelper::makeWorkflow2(
#   template = file.path(testDir, "SampleTest.yxmd"),
#   repl = list(
#     list(node = 2, data = inputs$the.data, type = 'input'),
#     list(node = 8, data = config, type = 'config'),
#     list(node = 15, data = coef_dframe, type = 'input'),
#     list(node = 17, data = comment, type = 'text'),
#     list(node = 16, data = 'Logistic Regression Test', type = 'text')
#   ),
#   outFile = file.path(testDir, "LogisticTest3.yxmd")
# )


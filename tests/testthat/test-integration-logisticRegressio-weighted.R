context("Logistic Regression: weighted version with api data")

config <- list(
  `graph.resolution` = '1x',
  `Link` = 'logit',
  `Model Name` = 'Basic_Logistic_Regression',
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

# AlteryxRhelper::makeWorkflow(
#   template = "~/Desktop/SNIPPETS/dev/Predictive_Refresh/Logistic_Regression/Extras/Tests/SampleTest.yxmd",
#   data = inputs$the.data,
#   config = config,
#   inputs_id = 2,
#   config_id = 8,
#   outFile = "~/Desktop/SNIPPETS/dev/Predictive_Refresh/Logistic_Regression/Extras/Tests/LogisticTest2.yxmd"
# )


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

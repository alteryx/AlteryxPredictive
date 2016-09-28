context("integration-linear-regression")

config <- list(
  `graph.resolution` = '1x',
  `Model Name` = 'My Regression',
  `Omit Constant` = FALSE,
  `Use Weights` = FALSE,
  `X Vars` = names(mtcars)[-1],
  `Y Var` = 'mpg'
)

inputs <- list(
  the.data = mtcars,
  XDFInfo = list(is_XDF = FALSE, xdf_path = NULL)
)

test_that('linear regression works correctly on mtcars', {
  results <- AlteryxPredictive:::runLinearRegression(inputs, config)
  exp_model <- lm(mpg ~ ., data = mtcars)
  expect_equal(results$Object[[1]]$coefficients, exp_model$coefficients)
})

testDir = '~/Desktop/SNIPPETS/dev/Predictive_Refresh/Linear_Regression/Extras/Tests/'
# AlteryxRhelper::makeWorkflow(
#   template = file.path(testDir, 'SampleTest.yxmd'),
#   data = inputs$the.data,
#   config = config,
#   inputs_id = 1,
#   config_id = 5,
#   outFile = file.path(testDir, 'LinearRegressionTest1.yxmd')
# )


config <- modifyList(config, list(
  `X Vars` = c('cut', 'color', 'price'),
  `Y Var` = 'carat'
))

inputs$the.data = head(ggplot2::diamonds[,c(config$`Y Var`, config$`X Vars`)], 200)

test_that('linear regression works correctly on diamonds', {
  results <- AlteryxPredictive:::runLinearRegression(inputs, config)
  exp_model <- lm(carat ~ cut + color + price, data = head(ggplot2::diamonds, 200))
  expect_equal(results$Object[[1]]$coefficients, exp_model$coefficients)
})

# AlteryxRhelper::makeWorkflow(
#   template = file.path(testDir, 'SampleTest.yxmd'),
#   data = inputs$the.data,
#   config = config,
#   inputs_id = 1,
#   config_id = 5,
#   outFile = file.path(testDir, 'LinearRegressionTest2.yxmd')
# )

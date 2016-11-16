context("integration-linear-regression")

config <- list(
  `graph.resolution` = '1x',
  `Model Name` = 'Linear_Regression_No_Weights',
  `Omit Constant` = FALSE,
  `Use Weights` = FALSE,
  `Weight Vec` = NULL,
  `X Vars` = c('disp', 'hp', 'drat', 'wt', 'qsec'),
  `Y Var` = 'mpg'
)

inputs <- list(
  the.data = mtcars[,c(config$`Y Var`, config$`X Vars`)],
  XDFInfo = list(is_XDF = FALSE, xdf_path = NULL)
)

exp_model <- lm(mpg ~ ., data = inputs$the.data)
test_that('linear regression works correctly on mtcars', {
  results <- AlteryxPredictive:::getResultsLinearRegression(inputs, config)
  expect_equal(results$model$coefficients, exp_model$coefficients)
})

coefs <- coef(exp_model)
coef_dframe <- data.frame(
  Variable = names(coefs),
  Coefficient = format(coefs, digits = 5)
)


# testDir = '~/Desktop/SNIPPETS/dev/Predictive_Tools/Linear_Regression/Extras/Tests/'
# comment = 'This workflow tests that mtcars data returns correct coefficients'
# AlteryxRhelper::makeWorkflow2(
#   template = file.path(testDir, "SampleTest.yxmd"),
#   repl = list(
#     list(node = 26, data = inputs$the.data, type = 'input'),
#     list(node = 3, data = config, type = 'config'),
#     list(node = 14, data = coef_dframe, type = 'input'),
#     list(node = 23, data = comment, type = 'text'),
#     list(node = 19, data = 'Linear Regression Test', type = 'text')
#   ),
#   outFile = file.path(testDir, "LinearTest1.yxmd")
# )


config <- modifyList(config, list(
  `X Vars` = c('cut', 'color', 'price'),
  `Y Var` = 'carat'
))

inputs$the.data = head(ggplot2::diamonds[,c(config$`Y Var`, config$`X Vars`)], 200)
#inputs$the.data = as.data.frame(inputs$the.data, stringsAsFactors = F)
inputs$the.data[,c('cut', 'color')] = lapply(
  inputs$the.data[,c('cut', 'color')], as.character
)

exp_model <- lm(carat ~ cut + color + price, data = inputs$the.data)
test_that('linear regression works correctly on diamonds', {
  results <- AlteryxPredictive:::getResultsLinearRegression(inputs, config)
  expect_equal(results$model$coefficients, exp_model$coefficients)
})


coefs <- coef(exp_model)
coef_dframe <- data.frame(
  Variable = names(coefs),
  Coefficient = format(coefs, digits = 5)
)

comment = 'This workflow tests that diamonds data returns correct coefficients'
# AlteryxRhelper::makeWorkflow2(
#   template = file.path(testDir, "SampleTest.yxmd"),
#   repl = list(
#     list(node = 26, data = inputs$the.data, type = 'input'),
#     list(node = 3, data = config, type = 'config'),
#     list(node = 14, data = coef_dframe, type = 'input'),
#     list(node = 23, data = comment, type = 'text'),
#     list(node = 19, data = 'Linear Regression Test', type = 'text')
#   ),
#   outFile = file.path(testDir, "LinearTest2.yxmd")
# )

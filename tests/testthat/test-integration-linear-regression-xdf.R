context("integration-linear-regression-xdf")

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
  XDFInfo = list(
    is_XDF = TRUE,
    xdf_path = system.file('xdfdata/mtcars.xdf', package = 'AlteryxPredictive')
  )
)

test_that('linear regression works correctly on mtcars', {
  skip_if_not_installed('RevoScaleR')
  results <- AlteryxPredictive:::runLinearRegression(inputs, config)
  exp_model <- lm(mpg ~ ., data = mtcars)
  expect_equal(
    results$Object[[1]]$coefficients[,1],
    exp_model$coefficients
  )
})

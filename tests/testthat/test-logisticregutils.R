context("Logistic Regression: helper functions testing ")

config <- list(
  `graph.resolution` = '1x',
  `the.link` = 'logit',
  `model.name` = 'my_model',
  `used.weights` = FALSE,
  `X Vars` = c('Sepal.Length', 'Petal.Length'),
  `Y Var` = 'Species'
)


defaults <- list(
  data = transform(iris,
                   Species = as.factor(ifelse(Species != "setosa", "other", "setosa"))
  )[,5:1]
)
inputs <- list(
  the.data = read.Alteryx2("#1", default = defaults$data)
)


test_that("processLogisticOSR", {
  result <- processLogisticOSR(inputs, config)
  expect_that(result$the.model, is_a("glm"))
  expect_identical(result$model_type, "binomial")
})




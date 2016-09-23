context("Logistic Regression: helper functions testing ")

config <- list(
  `graph.resolution` = dropdownInput('%Question.graph.resolution%' , '1x'),
  `the.link` = dropdownInput('%Question.Link%' , 'logit'),
  `model.name` = textInput('%Question.Model Name%', 'my_model'),
  `used.weights` = checkboxInput('%Question.Use Weights%' , FALSE),
  `Weight Vec` = dropdownInput('%Question.Weight Vec%'),
  `X Vars` = listInput('%Question.X Vars%', c('Sepal.Length', 'Petal.Length')),
  `Y Var` = dropdownInput('%Question.Y Var%', 'Species')
)

if (config$the.link == "complementary log-log"){
  config$the.link <- "cloglog"
}

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
  expect_identical("result$model_type", "binomial")
})




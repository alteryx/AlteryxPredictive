context("Decision Tree: smoke test with admission data")

config <- list(
  `Branch Dist` = checkboxInput('%Question.Branch Dist%' , TRUE),
  `classification` = radioInput('%Question.classification%' , TRUE),
  `Counts` = radioInput('%Question.Counts%' , FALSE),
  `cp` = numericInput('%Question.cp%' , 'Auto'),
  `max.bins` = textInput('%Question.max.bins%' , 'Default'),
  `max.depth` = numericInput('%Question.max.depth%' , 20),
  `min.bucket` = numericInput('%Question.min.bucket%' , 7),
  `min.split` = numericInput('%Question.min.split%' , 20),
  `Model Name` = textInput('%Question.Model Name%'),
  `percent.correct` = radioInput('%Question.percent.correct%' , FALSE),
  `Proportions` = radioInput('%Question.Proportions%' , TRUE),
  `prune.centimeters` = radioInput('%Question.prune.centimeters%' , FALSE),
  `prune.cm.h` = numericInput('%Question.prune.cm.h%' , 14.95),
  `prune.cm.w` = numericInput('%Question.prune.cm.w%' , 13),
  `prune.graph.resolution` = dropdownInput('%Question.prune.graph.resolution%' , '1x'),
  `prune.in.h` = numericInput('%Question.prune.in.h%' , 5.5),
  `prune.in.w` = numericInput('%Question.prune.in.w%' , 5.5),
  `prune.inches` = radioInput('%Question.prune.inches%' , TRUE),
  `prune.plot` = checkboxInput('%Question.prune.plot%' , FALSE),
  `prune.pointsize` = numericInput('%Question.prune.pointsize%' , 10),
  `regression` = radioInput('%Question.regression%' , FALSE),
  `select.type` = checkboxInput('%Question.select.type%' , FALSE),
  `select.weights` = dropdownInput('%Question.select.weights%'),
  `set_cp` = checkboxInput('%Question.set_cp%' , FALSE),
  `total.correct` = radioInput('%Question.total.correct%' , TRUE),
  `tree.centimeters` = radioInput('%Question.tree.centimeters%' , FALSE),
  `tree.cm.h` = numericInput('%Question.tree.cm.h%' , 14.95),
  `tree.cm.w` = numericInput('%Question.tree.cm.w%' , 13),
  `tree.graph.resolution` = dropdownInput('%Question.tree.graph.resolution%' , '1x'),
  `tree.in.h` = numericInput('%Question.tree.in.h%' , 5.5),
  `tree.in.w` = numericInput('%Question.tree.in.w%' , 5.5),
  `tree.inches` = radioInput('%Question.tree.inches%' , TRUE),
  `tree.plot` = checkboxInput('%Question.tree.plot%' , TRUE),
  `tree.pointsize` = numericInput('%Question.tree.pointsize%' , 8),
  `use.gini` = radioInput('%Question.use.gini%' , TRUE),
  `use.information` = radioInput('%Question.use.information%' , FALSE),
  `use.weights` = checkboxInput('%Question.use.weights%' , FALSE),
  `usesurrogate.0` = radioInput('%Question.usesurrogate.0%' , FALSE),
  `usesurrogate.1` = radioInput('%Question.usesurrogate.1%' , FALSE),
  `usesurrogate.2` = radioInput('%Question.usesurrogate.2%' , TRUE),
  `X Vars` = listInput('%Question.X Vars%', names(iris)[1:4]),
  `xval.folds` = numericInput('%Question.xval.folds%' , 10),
  `Y Var` = dropdownInput('%Question.Y Var%', 'Species')
)

inputs <- list(
  the.data = iris,
  XDFInfo = list(is_XDF = FALSE, xdf_path = NULL)
)

# the.controls <- paste(', minsplit = ', %Question.min.split%, ',
#                       minbucket = ', %Question.min.bucket%, ',
#                       usesurrogate = ', use.surrogate, ',
#                       xval = ', %Question.xval.folds%, ',
#                       maxdepth = ', %Question.max.depth%, sep = "")
# model.call <- paste(model.name, ' <- rpart(', the.formula, weight.arg, ', data = the.data', method.split, the.controls, sep = "")

#' ### Run and Create Outputs
exp_tree_model <- rpart(
  formula = Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
  data = iris
)

test_that("Iris data with vanilla decision tree model", {
  result <- AlteryxPredictive:::runDecisionTree(inputs, config)
  expect_equal(
    result$frame,
    exp_tree_model$frame
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

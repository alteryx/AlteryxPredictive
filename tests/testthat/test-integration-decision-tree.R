context("Decision Tree: smoke test with admission data")

config <- list(
  `Branch Dist` = TRUE, classification = TRUE, Counts = FALSE,
  cp = 0.01, max.bins = "Default", max.depth = 20, min.bucket = 7,
  min.split = 20, `Model Name` = "Decision_Tree", percent.correct = FALSE,
  Proportions = TRUE, prune.centimeters = FALSE, prune.cm.h = 14.95,
  prune.cm.w = 13, prune.graph.resolution = "1x", prune.in.h = 5.5,
  prune.in.w = 5.5, prune.inches = TRUE, prune.plot = FALSE,
  prune.pointsize = 10, regression = FALSE, select.type = FALSE,
  select.weights = "", set_cp = FALSE, total.correct = TRUE,
  tree.centimeters = FALSE, tree.cm.h = 14.95, tree.cm.w = 13,
  tree.graph.resolution = "1x", tree.in.h = 5.5, tree.in.w = 5.5,
  tree.inches = TRUE, tree.plot = TRUE, tree.pointsize = 8,
  use.gini = TRUE, use.information = FALSE, use.weights = FALSE,
  usesurrogate.0 = FALSE, usesurrogate.1 = FALSE, usesurrogate.2 = TRUE,
  `X Vars` = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  xval.folds = 10, `Y Var` = "Species", model.algorithm = "rpart",
  display.static = TRUE, trials = 1, rules = FALSE, subset = TRUE,
  bands.check = FALSE, bands = 10, winnow = FALSE, GlobalPruning = TRUE,
  CF = .25, minCases = 2, fuzzyThreshold = FALSE, sample = 0, seed = 1,
  earlyStopping = TRUE
)

inputs <- list(
  the.data = iris[, c(config$`Y Var`, config$`X Vars`)],
  XDFInfo = list(is_XDF = FALSE, xdf_path = NULL)
)


#' ### Run and Create Outputs
exp_tree_model <- rpart::rpart(
  formula = Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
  data = iris,
  maxdepth = 20
)


test_that("Iris data with vanilla decision tree model", {
  result <- AlteryxPredictive:::getResultsDecisionTree(inputs, config)
  expect_equal(getCall(result$model)[[2]], getCall(exp_tree_model)[[2]])
  expect_equal(result$model$frame,exp_tree_model$frame)
})

config$model.algorithm <- "C5.0"

exp_C50_model <- C50::C5.0(formula = Species ~ ., data = iris)

test_that("Iris data with vanilla C5.0 tree model", {
  result <- AlteryxPredictive:::getResultsDecisionTree(inputs, config)
  expect_equal(result$model$tree, exp_C50_model$tree)
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

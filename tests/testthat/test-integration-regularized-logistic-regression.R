context("Logistic Regression: regularized version with admission data")

config <- list(
  `graph.resolution` = '1x',
  `Link` = 'logit',
  `Model Name` = 'Basic_Regularized_Logistic_Regression',
  `Use Weights` = FALSE,
  `Weight Vec` = NULL,
  `X Vars` = c('gre', 'gpa'),
  `Y Var` = 'admit',
  regularization = checkboxInput('%Question.regularization%', TRUE),
  alpha = numericInput('%Question.alpha%', .5),
  lambda_1se = radioInput('%Question.lambda.1se%', TRUE),
  lambda_min = radioInput('%Question.lambda.min%', FALSE),
  standardize_pred = checkboxInput('%Question.standardize_pred%', TRUE),
  internal_cv = checkboxInput('%Question.internal_cv%', FALSE),
  nfolds = numericInput('%Question.nfolds%', 5),
  lambda_no_cv = numericInput('%Question.lambda_no_cv%', 1),
  display_graphs = checkboxInput('%Question.display_graphs%', TRUE),
  external_cv = checkboxInput('%Question.external_cv%', FALSE),
  `Omit Constant` = checkboxInput('%Question.Omit Constant%', FALSE)
)

inputs <- list(
  the.data = AlteryxPredictive::admission[, c(config$`Y Var`, config$`X Vars`)],
  XDFInfo = list(is_XDF = FALSE, xdf_path = NULL)
)

#' ### Run and Create Outputs
exp_logistic_model <- glmnet(
  x = as.matrix((inputs$the.data)[,(config$`X Vars`)]),
  y = (inputs$the.data)[[config$`Y Var`]],
  family = "binomial",
  alpha = .5
)

test_that('regularized linear regression works correctly on mtcars', {
  results <- AlteryxPredictive:::getResultsLogisticRegression(inputs, config)
  temp_coefs <- coef(exp_logistic_model, s = 1, exact = FALSE)
  vector_coefs_out <- as.vector(temp_coefs)
  expect_equal(results$coefficients,
               data.frame(Coefficients = rownames(temp_coefs), Values = vector_coefs_out))
})

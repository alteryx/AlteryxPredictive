context("integration-regularized-linear-regression")

config <- list(
  `graph.resolution` = dropdownInput('%Question.graph.resolution%' , '1x'),
  `Model Name` = textInput('%Question.Model Name%', "mtcars"),
  `Omit Constant` = checkboxInput('%Question.Omit Constant%' , FALSE),
  `Use Weights` = checkboxInput('%Question.Use Weights%' , FALSE),
  `Weight Vec` = dropdownInput('%Question.Weight Vec%'),
  `X Vars` = listInput('%Question.X Vars%', names(mtcars)[-1]),
  `Y Var` = dropdownInput('%Question.Y Var%', 'mpg'),
  regularization = checkboxInput('%Question.regularization%', TRUE),
  alpha = numericInput('%Question.alpha%', .5),
  lambda_1se = radioInput('%Question.lambda.1se%', TRUE),
  lambda_min = radioInput('%Question.lambda.min%', FALSE),
  standardize_pred = checkboxInput('%Question.standardize_pred%', TRUE),
  internal_cv = checkboxInput('%Question.internal_cv%', FALSE),
  nfolds = numericInput('%Question.nfolds%', 5),
  lambda_no_cv = numericInput('%Question.lambda_no_cv%', 1),
  display_graphs = checkboxInput('%Question.display_graphs%', TRUE)
)

inputs <- list(
  the.data = mtcars[,c(config$`Y Var`, config$`X Vars`)],
  XDFInfo = list(is_XDF = FALSE, xdf_path = NULL)
)

exp_model <- glmnet::glmnet(x = as.matrix((inputs$the.data)[,(config$`X Vars`)]), y = (inputs$the.data)$mpg,
                            family = 'gaussian', alpha = .5, standardize = TRUE, intercept=TRUE)
test_that('regularized linear regression works correctly on mtcars', {
  results <- AlteryxPredictive:::getResultsLinearRegression(inputs, config)
  temp_coefs <- coef(exp_model, s = 1, exact = FALSE)
  vector_coefs_out <- as(temp_coefs, "vector")
  expect_equal(results$coefficients,
               data.frame(Names_of_nonzero_coefficients = rownames(temp_coefs), Coefficient_values = vector_coefs_out))
})


config2 <- list(
  `graph.resolution` = dropdownInput('%Question.graph.resolution%' , '1x'),
  `Model Name` = textInput('%Question.Model Name%', "mtcars"),
  `Omit Constant` = checkboxInput('%Question.Omit Constant%' , FALSE),
  `Use Weights` = checkboxInput('%Question.Use Weights%' , FALSE),
  `Weight Vec` = dropdownInput('%Question.Weight Vec%'),
  `X Vars` = listInput('%Question.X Vars%', names(mtcars)[-1]),
  `Y Var` = dropdownInput('%Question.Y Var%', 'mpg'),
  regularization = checkboxInput('%Question.regularization%', TRUE),
  alpha = numericInput('%Question.alpha%', .5),
  lambda_1se = radioInput('%Question.lambda.1se%', TRUE),
  lambda_min = radioInput('%Question.lambda.min%', FALSE),
  standardize_pred = checkboxInput('%Question.standardize_pred%', TRUE),
  internal_cv = checkboxInput('%Question.internal_cv%', TRUE),
  nfolds = numericInput('%Question.nfolds%', 5),
  lambda_no_cv = numericInput('%Question.lambda_no_cv%', 1),
  display_graphs = checkboxInput('%Question.display_graphs%', TRUE)
)

inputs2 <- list(
  the.data = mtcars[,c(config2$`Y Var`, config2$`X Vars`)],
  XDFInfo = list(is_XDF = FALSE, xdf_path = NULL)
)

#Since the results will depend on the folds used with cross-validation, we need to set the seed
#before both runs.
set.seed(1)
exp_model <- glmnet::cv.glmnet(x = as.matrix((inputs2$the.data)[,(config$`X Vars`)]), y = (inputs2$the.data)$mpg,
                               family = 'gaussian', alpha = .5, standardize = TRUE, intercept=TRUE, nfolds = 5)
test_that('regularized linear regression with internal CV works correctly on mtcars', {
  set.seed(1)
  results <- AlteryxPredictive:::getResultsLinearRegression(inputs2, config2)
  temp_coefs <- coef(exp_model, s = "lambda.1se", exact = FALSE)
  vector_coefs_out <- as(temp_coefs, "vector")
  expect_equal(results$coefficients,
               data.frame(Names_of_nonzero_coefficients = rownames(temp_coefs), Coefficient_values = vector_coefs_out))
})

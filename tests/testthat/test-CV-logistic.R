outer_config <- list(
  `graph.resolution` = '1x',
  `Link` = 'logit',
  `Model Name` = "logreg_model",
  `Use Weights` = FALSE,
  `Weight Vec` = NULL,
  `X Vars` = c('gre', 'gpa', 'rank'),
  `Y Var` = 'admit',
  regularization = FALSE,
  alpha = .5,
  lambda_1se = TRUE,
  lambda_min = FALSE,
  standardize_pred = TRUE,
  internal_cv = TRUE,
  set_seed_internal_cv = TRUE,
  seed_internal_cv = 1,
  nfolds = 5,
  lambda_no_cv = NULL,
  display_graphs = TRUE,
  external_cv = TRUE,
  nfolds_external = NULL,
  set_seed_external_cv = FALSE,
  external_seed_value = NULL,
  `Omit Constant` = FALSE
)



config <- list(
  `classification` = TRUE,
  `modelType` = NULL,
  `numberFolds` = 5,
  `numberTrials` = 3,
  `posClass` = NULL,
  `regression` = FALSE,
  `stratified` = FALSE,
  `seed` = 1
)

config <- append(config, outer_config)

inputs <- list(
  data = admission[,c(config$`X Vars`, config$`Y Var`)],
  models = list(model = glm(
    formula = admit ~ .,
    data = admission,
    family = binomial()
  ))
)

AlteryxPredictive:::runCrossValidationLogReg(inputs, config)

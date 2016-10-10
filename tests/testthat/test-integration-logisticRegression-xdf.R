context("Logistic Regression: XDF version with admission data")

config <- list(
  `graph.resolution` = '1x',
  `Link` = 'logit',
  `Model Name` = 'Basic_Logistic_Regression',
  `Use Weights` = FALSE,
  `Weight Vec` = NULL,
  `X Vars` = c('gre', 'gpa', 'rank'),
  `Y Var` = 'admit'
)

inputs <- list(
  the.data = AlteryxPredictive::admission[, c(config$`Y Var`, config$`X Vars`)],
  XDFInfo = list(
    is_XDF = TRUE,
    xdf_path = system.file('xdfdata/admission.xdf', package = 'AlteryxPredictive')
  )
)

exp_logistic_model <- glm(
  admit ~ gre + gpa + rank, data = admission,
  family = binomial(logit)
)

test_that("admission data with logit link returns correct coefficients", {
  result <- AlteryxPredictive:::runLogisticRegression(inputs, config)
  expect_equal(
    result$Object[[1]]$coefficients,
    exp_logistic_model$coefficients
  )
})

d <- AlteryxPredictive:::runLogisticRegression(inputs, config)

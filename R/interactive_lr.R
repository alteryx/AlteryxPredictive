#' Dashboard generation for logistic regression models
#'
#' @param config list of configuration elements
#' @param data dataframe
#' @param model model object - should be one of
#'  glm - with binomial family
#'  glmnet
#'  cv.glmnet
#' @return dashboard object for rendering
#' @export
#' @author Todd Morley
interactive_lr <- function(
  config,
  data,
  model
){
  requireNamespace("flightdeck")
  # optimal cutoff probability from ROC analysis,
  # weighing sensitivity and specificity equally;
  # returns a named vector:  sensitivity, specificity,
  # and optimal_cutoff
  optimal_cutoff <- function(
    perf,
    pred
  ){
    cut.ind = mapply(FUN = function(x, y, p){
      d = (x - 0)^2 + (y - 1)^2
      ind = which(d == min(d))
      c(
        sensitivity = y[[ind]],
        specificity = 1 - x[[ind]],
        optimal_cutoff = p[[ind]])
    },
    perf@x.values,
    perf@y.values,
    pred@cutoffs
    )
  }

  # UI layout constants

  totalWidth <- 12
  halfWidth <- 6
  digits <- 3

  # Prep and test inputs.
  glm_b <- FALSE
  regularized_b <- FALSE
  cv_b <- FALSE
  if('glm' %in% class(model)){
    glm_b <- TRUE
    title <- 'Classical Logistic Regression'
  } else if(any(c('reg_glm', 'glmnet') %in% class(model))){
    regularized_b <- TRUE
    title <- 'Regularized Logistic Regression'
  } else if(any(c('cv.glmt', 'cv.glmnet') %in% class(model))){
    cv_b <- TRUE
    title <- 'Cross-Validated Logistic Regression'
  } else{
    stop.Alteryx2(
      paste(
        'An invalid model type was passed to interactive_lm. ',
        'Please contact Alteryx support!'
      )
    )
  }
  logistic_b <- FALSE
  probit_b <- FALSE
  log_log_b <- FALSE
  if (glm_b) {
    if(
      model$family$family == 'binomial'
    ){
      if(model$family$link == 'logit'){
        logistic_b <- TRUE
        link_function <- 'logit'
      } else if(model$family$link == 'probit'){
        probit_b <- TRUE
        link_function <- 'probit'
      } else if(model$family$link == 'cloglog'){
        log_log_b <- TRUE
        link_function <- 'complementary log log'
      } else{
        stop.Alteryx2(
          paste(
            'An invalid link function was passed to interactive_lr. ',
            'Please contact Alteryx support!')
        )
      }
    } else{
      stop.Alteryx2(
        paste(
          'An invalid model family was passed to interactive_lr. ',
          'Please contact Alteryx support!'
        )
      )
    }
  }

  the_actual_values <- data[, 1]
  fitted_intercept <- !config$`Omit Constant`
  alpha <- config$alpha
  use_cv_lambda_1se <- config$lambda_1se
  lambda <- config$lambda_no_cv
  n <- nrow(data)
  p <- ncol(data) - 1 - as.numeric(config$`Use Weights`)

  # model-summary numbers

  if(glm_b){
    the_fitted_values <- unname(model$fitted.values)
  } else{
    independent_variable_m <- sapply(
      X = unname(data[, -1]),
      FUN = as.numeric,
      simplify = 'array'
    )
    if(regularized_b){
      lambda <- config$lambda_no_cv
    } else{
      if(use_cv_lambda_1se){
        lambda <- model$lambda.1se
      } else{
        lambda <- model$lambda.min
      }
    }
    the_fitted_values <- unname(
      predict(
        object = model,
        newx = independent_variable_m,
        s = lambda,
        type = 'response'
      )
    )
  }

  use_sampling_weights_b <- config$`Use Weights`
  n <- nrow(data)
  p <- ncol(data) - 1 - as.numeric(use_sampling_weights_b)
  actual_values <- data[, 1]
  if (is.factor(actual_values)) {
    actual_values <- as.numeric(actual_values) - 1
  }
  actual_values_f <- factor(
    actual_values,
    levels = 0:1,
    labels = c('no', 'yes')
  )
  # probability_v <- if (glm_b) {
  #   predict(
  #     object = model,
  #     type = 'response'
  #   )
  # } else {
  #   predict(
  #     object = model,
  #     type = 'response',
  #     newx = df2NumericMatrix(data[,1]),
  #     s = model$lambda
  #   )
  # }
  # saveRDS(list(predictions = probability_v, labels = actual_values),
  #         "C:\\Users\\dblanchard\\Documents\\playground\\logregdash\\prediction_params.rds")
  #

  probability_v <- the_fitted_values

  # ROCR computations
  prediction_object <- ROCR::prediction(
    predictions = probability_v,
    labels = actual_values
  )
  roc_performance = ROCR::performance(
    prediction.obj = prediction_object,
    measure = 'tpr',
    x.measure = 'fpr'
  )
  optimal_cutoff_nv <- optimal_cutoff(
    perf = roc_performance,
    pred = prediction_object
  )
  fitted_values <- as.integer(probability_v >= config$threshold)
  true_positive_count <- length(
    intersect(
      which(fitted_values == 1),
      which(actual_values == 1)
    )
  )
  true_positive_percent <- round(
    true_positive_count * 100 / n,
    digits
  )
  false_positive_count <- length(which(fitted_values > actual_values))
  false_positive_percent <- round(
    false_positive_count * 100 / n,
    digits
  )
  true_negative_count <- length(
    intersect(
      which(fitted_values == 0),
      which(actual_values == 0)
    )
  )
  true_negative_percent <- round(
    true_negative_count * 100 / n,
    digits
  )
  false_negative_count <- length(which(fitted_values < actual_values))
  false_negative_percent <- round(
    false_negative_count * 100 / n,
    digits
  )
  confusion_matrix_m <- matrix(
    data = c(
      true_positive_count,
      false_positive_count,
      false_negative_count,
      true_negative_count
    ),
    nrow = 2,
    ncol = 2
  )
  rownames(confusion_matrix_m) <- c('Predicted Positive', 'Predicted Negative')
  colnames(confusion_matrix_m) <- c('Actual Positive', 'Actual Negative')

  # Prepare UI elements.

  # page 1:  model summary
  row_1_1 <- fdRow(
    fdBox(
      fdPanelClassificationMetrics(
        actual = actual_values,
        predicted = fitted_values,
        metrics = c("Accuracy", "Recall", "Precision", "F1_Score")
      ),
      width = totalWidth
    )
  )
  row_1_2 <- fdRow(
    fdBox(
      fdPlotConfusionMatrix(x = confusion_matrix_m),
      width = totalWidth
    )
  )
  page_1 <- fdPage(
    row_1_1,
    row_1_2,
    id = 'page_1',
    display = TRUE
  )

  # page 2:  conditional-density plots

  independent_variables <- names(data[, -1])
  cd_plots <- lapply(
    independent_variables,
    function(x){
      plt <- fdPlotConditionalDensity(
        x = data[[x]],
        y = actual_values_f,
        xlab = x,
        showlegend = F
      )
      fdColumn(
        plt,
        width = halfWidth
      )
    }
  )
  row_2_1 <- fdRow(
    fdBox(
      cd_plots,
      width = totalWidth
    )
  )
  page_2 <- fdPage(
    row_2_1,
    id = 'page_2',
    display = FALSE
  )

  # page 3: advanced diagnostics

  roc_chart <- fdPlotClassificationPerformance(
    performance(
      prediction_object,
      "tpr",
      "fpr"
    ),
    digits = digits
  )
  precision_recall_chart <- fdPlotClassificationPerformance(
    performance(
      prediction_object,
      "prec",
      "rec"
    ),
    digits = digits
  )
  row_3_1 <- fdRow(
    fdBox(
      fdTabsetPanel(
        selected = makeHtmlId('ROC Chart'),
        fdTabPanel(
          'ROC Chart',
          roc_chart
        ),
        fdTabPanel(
          'Precision vs. Recall',
          precision_recall_chart
        )
      ),
      width = totalWidth
    )
  )
  page_3 <- fdPage(
    row_3_1,
    id = 'page_3',
    display = FALSE
  )

  # render

  the_header <- fdHeader(title = title)
  sidebar <- fdSidebarMenu(
    fdMenuItem(
      text = 'Summary',
      icon = fdIcon(
        name = 'caret-right',
        lib = "font-awesome"
      ),
      pageName = 'page_1'
    ),
    fdMenuItem(
      text = 'Conditional-Density Plots',
      icon = fdIcon(
        name = 'caret-right',
        lib = "font-awesome"
      ),
      pageName = 'page_2'
    ),
    fdMenuItem(
      text = 'Performance',
      icon = fdIcon(
        name = 'caret-right',
        lib = "font-awesome"
      ),
      pageName = 'page_3'
    )
  )
  body <- fdBody(
    page_1,
    page_2,
    page_3
  )
  fdBoard(
    fdHeader(
      title = title,
      titleWidth = 600),
    fdSidebar(sidebar),
    body,
    fixed = TRUE
  )

}

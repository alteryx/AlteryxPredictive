#' Dashboard generation for logistic regression models
#'
#' @param config list of configuration elements
#' @param data dataframe
#' @param model model object - should be one of
#'  glm - with binomial family
#'  glmnet
#'  cv.glmnet
#' @param cv_metrics metrics returned by runCrossValidationLogReg
#' @return dashboard object for rendering
#' @export
#' @author Todd Morley
interactive_lr <- function(
  config,
  data,
  model,
  cv_metrics = NULL
){
  if(config$`Use Weights`) {
    data <- data[,-NCOL(data)]
  }
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
    return(badDash(
      paste0(
        'Interactive visualization not available for models of class ',
        class(model),
        '.'
      )
    ))
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
        return(badDash(
          paste(
            'An invalid link function was passed to interactive_lr. ',
            'Please contact Alteryx support!')
        ))
      }
    } else{
      return(badDash(
        paste(
          'An invalid model family was passed to interactive_lr. ',
          'Please contact Alteryx support!'
        )
      ))
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
    independent_variable_m <- df2NumericMatrix(
      x = data[ , -1, drop = FALSE],
      filtering_message = "Non-numeric variables are among the predictors. They are now being removed.",
      convertVectorToDataFrame = TRUE
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
    if(all(unlist(model$coefficients[-1] == 0))){
      msg1 <- "All model coefficients were zero. Cannot generate dashboard. "
      if(regularized_b) {
        msg2 <- "Consider using a smaller value of lambda."
      } else { # cv_b is true
        if (config$lambda_1se) {
          msg2 <- "Consider using lambda.min instead of lambda for simple model."
        } else {
          msg2 <- ""
        }
      }
      return(badDash(paste0(msg1,msg2)))
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
  p <- ncol(data) - 1
  actual_values <- data[, 1]
  if (is.factor(actual_values)) {
    actual_values <- as.numeric(actual_values) - 1
  }
  actual_values_f <- factor(
    actual_values,
    levels = 0:1,
    labels = c('no', 'yes')
  )
  probability_v <- the_fitted_values

  # ROCR computations
  prediction_object <- ROCR::prediction(
    predictions = probability_v,
    labels = actual_values
  )
  roc_performance <- ROCR::performance(
    prediction.obj = prediction_object,
    measure = 'tpr',
    x.measure = 'fpr'
  )
  optimal_cutoff_nv <- optimal_cutoff(
    perf = roc_performance,
    pred = prediction_object
  )
  fitted_values <- as.integer(probability_v >= optimal_cutoff_nv[3])
  if(length(unique(fitted_values)) == 1) {
    msg1 <- "All values are being fitted to the same class. "
    if(regularized_b) {
      msg2 <- "Consider using a smaller value of lambda. "
    }
    else if(cv_b) {
      msg2 <- "Consider using a different value of lambda. "
    }
    msg3 <- "Interactive dashboard could not be generated."
    return(badDash(paste0(msg1,msg2,msg3)))
  }
  if(is.null(x = cv_metrics)){
    true_positive_count <- length(
      intersect(
        which(fitted_values == 1),
        which(actual_values == 1)
      )
    )
    true_negative_count <- length(
      intersect(
        which(fitted_values == 0),
        which(actual_values == 0)
      )
    )
    false_positive_count <- length(which(fitted_values > actual_values))
    false_negative_count <- length(which(fitted_values < actual_values))
    accuracy <- (true_positive_count + true_negative_count) / n
    precision <- true_positive_count / (true_positive_count + false_positive_count)
    recall <- true_positive_count / (true_positive_count + false_negative_count)
    # This should be a harmonic mean.  It could be NaN, which for now is OK.
    f1 <- 1 / mean(1 / c(precision, recall))
  } else{
    true_positive_count <- cv_metrics['pred_pos_actual_pos']
    true_negative_count <- cv_metrics['pred_neg_actual_neg']
    false_positive_count <- cv_metrics['pred_pos_actual_neg']
    false_negative_count <- cv_metrics['pred_neg_actual_pos']
    accuracy <- cv_metrics['accuracy']
    precision <- cv_metrics['precision']
    recall <- cv_metrics['recall']
    f1 <- cv_metrics['f1']
  }
  confusion_matrix_m <- matrix(
    data = c(
      round(x = true_positive_count, digits = digits),
      round(x = false_negative_count, digits = digits),
      round(x = false_positive_count, digits = digits),
      round(x = true_negative_count, digits = digits)
    ),
    nrow = 2,
    ncol = 2
  )
  rownames(confusion_matrix_m) <- c('Predicted Positive', 'Predicted Negative')
  colnames(confusion_matrix_m) <- c('Actual Positive', 'Actual Negative')

  # Prepare UI elements.
  # page 1:  model summary

  row_1_1 <- fdRow(
    fdInfoBox(
      title = 'Accuracy',
      value = round(
        x = accuracy,
        digits = digits
      ),
      icon = fdIcon(
        name = 'check',
        lib = 'font-awesome'
      ),
      color = 'blue',
      width = halfWidth
    ),
    fdInfoBox(
      title = 'Precision',
      value = round(
        x = precision,
        digits = digits
      ),
      icon = fdIcon(
        name = 'check',
        lib = 'font-awesome'
      ),
      color = 'blue',
      width = halfWidth
    )
  )
  row_1_2 <- fdRow(
    fdInfoBox(
      title = 'Recall',
      value = round(
        x = recall,
        digits = digits
      ),
      icon = fdIcon(
        name = 'check',
        lib = 'font-awesome'
      ),
      color = 'blue',
      width = halfWidth
    ),
    fdInfoBox(
      title = 'F1',
      value = round(
        x = f1,
        digits = digits
      ),
      icon = fdIcon(
        name = 'check',
        lib = 'font-awesome'
      ),
      color = 'blue',
      width = halfWidth
    )
  )
  row_1_3 <- fdRow(
    fdInfoBox(
      title = 'Optimal Probability Cutoff',
      value = round(
        x = optimal_cutoff_nv[3],
        digits = digits
      ),
      icon = fdIcon(
        name = 'check',
        lib = 'font-awesome'
      ),
      color = 'blue',
      width = totalWidth
    )
  )
  row_1_4 <- fdRow(
    fdBox(
      fdPlotConfusionMatrix(x = confusion_matrix_m),
      width = totalWidth
    )
  )
  page_1 <- fdPage(
    row_1_1,
    row_1_2,
    row_1_3,
    row_1_4,
    id = 'page_1',
    display = TRUE
  )

  # page 2:  conditional-density plots
  independent_variables <- names(data[, -1])
  cd_plots <- lapply(
    independent_variables,
    function(x){
      if(is.numeric(data[[x]])){
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
      } else {
        print(
          paste0(
            "Conditional-density plot not generated for variable '",
            x,
            "' because it was categorical."
            )
          )
      }
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
  titleWidth <- computeWidth(title)
  fdBoard(
    fdHeader(
      title = title,
      titleWidth = titleWidth
    ),
    fdSidebar(
      sidebar,
      sidebarWidth = titleWidth
    ),
    body,
    fixed = TRUE
  )
}

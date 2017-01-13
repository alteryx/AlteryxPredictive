#' Dashboard generation for decision tree models
#'
#' @param config list of configuration elements
#' @param data dataframe
#' @param model model object - should be one of
#'  rpart
#'  C5.0
#' @author Todd Morley
#' @return dashboard object for rendering
#' @export
#' @author Todd Morley
interactive_dt <- function(
  config,
  data,
  model
) {
  if(config$used.weights) {
    data <- data[,-NCOL(data)]
  }
  requireNamespace("flightdeck")
  # test inputs
  rpart_regression_b <- FALSE
  rpart_classification_b <- TRUE
  c50_b <- FALSE

  # UI layout constants

  totalWidth <- 12
  halfWidth <- 6
  digits <- 3

  # Prep and test inputs.

  fitted_intercept <- TRUE
  rpart_regression_b <- FALSE
  rpart_classification_b <- FALSE
  c50_b <- FALSE
  use_sampling_weights_b <- config$used.weights
  n <- nrow(data)
  p <- ncol(data) - 1
  actual_values <- data[, 1]
  if('rpart' %in% class(model)){
    if(model$method == 'anova'){
      rpart_regression_b <- TRUE
      title <- 'RPart Decision-Tree Regression'
      fitted_values <- predict(
        object = model,
        newdata = data,
        type = 'vector'
      )
      if(fitted_intercept){
        intercept_degrees_freedom <- 1
      } else{
        intercept_degrees_freedom <- 0
      }
      r_squared <- R2_Score(
        y_pred = fitted_values,
        y_true = actual_values
      )
      adj_r_squared <- adj_r_squared(
        r_squared = r_squared,
        n = n,
        p = p,
        intercept_degrees_freedom = intercept_degrees_freedom
      )
      residuals <- unname(actual_values - fitted_values)
    } else if(model$method == 'class'){
      rpart_classification_b <- TRUE
      title <- 'RPart Decision-Tree Classification'
      fitted_values <- unname(
        predict(
          object = model,
          newdata = data,
          type = 'class'
        )
      )
    } else{
      return(badDash(
        'Interactive visualization not available rpart model without method "class" or "anova" '
      ))
    }
  } else if('C5.0' %in% class(model)){
    c50_b <- TRUE
    title <- 'C5.0 Decision-Tree Classification'
    fitted_values <- unname(
      predict(
        object = model,
        newdata = data,
        trials =  model$trials["Actual"],
        type = 'class'
      )
    )
  } else{
    return(badDash(
      paste0(
        'Interactive visualization not available for models of class ',
        class(model),
        '.'
      )
    ))
  }
  if(rpart_classification_b || c50_b){
    actual_values_f <- as.factor(actual_values)
    mismatch_t <- table(
      actual = actual_values_f,
      predicted = fitted_values
    )
  }

  # UI

  if(rpart_regression_b){ # rpart regression UI
    # page 1:  summary
    row_1_1 <- fdRow(
      fdInfoBox(
        title = 'R Squared',
        value = round(
          x = r_squared,
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
        title = 'Adjusted R Squared',
        value = round(
          x = adj_r_squared,
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
        title = 'Mean Absolute Error',
        value = round(
          x = MAE(
            y_pred = fitted_values,
            y_true = actual_values
          ),
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
        title = 'Mean Absolute Percent Error',
        value = round(
          x = MAPE(
            y_pred = fitted_values,
            y_true = actual_values
          ),
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
        title = 'Mean Squared Error',
        value = round(
          x = MSE(
            y_pred = fitted_values,
            y_true = actual_values
          ),
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
        title = 'Root Mean Squared Error',
        value = round(
          x = RMSE(
            y_pred = fitted_values,
            y_true = actual_values
          ),
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
    page_1 <- fdPage(
      row_1_1,
      row_1_2,
      row_1_3,
      id = 'page_1',
      display = TRUE
    )
    # page 2:  performance
    row_2_1 <- fdRow(
      fdBox(
        fdPanelRegressionMetrics(
          actual = actual_values,
          predicted  = fitted_values,
          metrics = c("MAE", "MAPE", "MedianAPE", "RMSE", "RAE", "R2_Score") # not "RMSLE"
        ),
        width = totalWidth
      )
    )
    row_2_2 <- fdRow(
      fdBox(
        fdPanelHistogram(
          x = residuals,
          digits = digits,
          plotTitle = 'Histogram of Residuals'
        ),
        width = totalWidth
      )
    )
    page_2 <- fdPage(
      row_2_1,
      row_2_2,
      id = 'page_2',
      display = FALSE
    )
    # page 3:  variable importance
    # rpart_variable_importance_v
    row_3_1 <- fdRow(
      fdBox(
        fdPanelImportance(
          mod = model,
          digits = digits,
          barColor = 'steelblue'
        ),
        width = totalWidth
      )
    )
    page_3 <- fdPage(
      row_3_1,
      id = 'page_3',
      display = FALSE
    )
    body <- fdBody(
      page_1,
      page_2,
      page_3
    )
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
        text = 'Model Performance',
        icon = fdIcon(
          name = 'caret-right',
          lib = "font-awesome"
        ),
        pageName = 'page_2'
      ),
      fdMenuItem(
        text = 'Variable Importance',
        icon = fdIcon(
          name = 'caret-right',
          lib = "font-awesome"
        ),
        pageName = 'page_3'
      )
    )
  } else{ # classification UI
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
    page_1 <- fdPage(
      row_1_1,
      id = 'page_1',
      display = TRUE
    )

    if(all.equal(
      length(levels(actual_values)),
      length(levels(fitted_values)),
      2
    )){
      # 2 classes
      confusion_matrix_m <- getBinaryConfusionMatrix(fitted_values, actual_values)

      row_2_1 <- fdRow(
        fdBox(
          fdPlotConfusionMatrix(x = confusion_matrix_m),
          width = totalWidth
        )
      )
    } else {
      # More than 2 classes
      row_2_1 <- fdRow(
        fdBox(
          fdPlotMismatchMatrix(
            x = mismatch_t,
            digits = digits
          ),
          width = totalWidth
        )
      )
    }

    page_2 <- fdPage(
      row_2_1,
      id = 'page_2',
      display = FALSE
    )
    if(rpart_classification_b){
      row_3_1 <- fdRow(
        fdBox(
          AlteryxRviz::renderTree(
            fit = model
            #, colpal =
          ),
          width = totalWidth
        )
      )
      page_3 <-fdPage(
        row_3_1,
        id = 'page_3',
        display = FALSE
      )
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
          text = 'Misclassifications',
          icon = fdIcon(
            name = 'caret-right',
            lib = "font-awesome"
          ),
          pageName = 'page_2'
        ),
        fdMenuItem(
          text = 'Tree',
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

    } else{
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
          text = 'Misclassifications',
          icon = fdIcon(
            name = 'caret-right',
            lib = "font-awesome"
          ),
          pageName = 'page_2'
        )
      )
      body <- fdBody(
        page_1,
        page_2
      )
    }
  }
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

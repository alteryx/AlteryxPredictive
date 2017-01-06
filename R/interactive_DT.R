
#' The classification_mismatches function produces a sparse confusion
#' matrix listing misclassifications in descending order of frequency,
#' and limiting its output to some total cumulative percentage of
#' misclassifications (in case there's a long tail of low-probability
#' misclassifications).  This lets the user try to alter the model
#' in ways calculated to achieve improvement around specific, frequently
#' occurring cases of misclassification.
#'
#' @param actual_values vector of expected values
#' @param fitted_values vector of fitted values
#' @param digits number of digits past the decimal to round to
#' @param cutoff_cumulative_percent cumulative percentage at which to stop outputting
#' @return dataframe with at least one row truncated at the cumulative percent
#' @author Todd Morley
classification_mismatches <- function(
  actual_values,
  fitted_values,
  digits = 3,
  cutoff_cumulative_percent = 80
){
  # defensive coding to bounds check cutoff_cumulative_percent
  if(
    cutoff_cumulative_percent < 20 ||
    cutoff_cumulative_percent > 100
  ){
    cutoff_cumulative_percent <- 80
  }
  # actual vs fitted data frame
  actual_fitted_df <- data.frame(
    actual_values = actual_values[which(fitted_values != actual_values)],
    fitted_values = fitted_values[which(fitted_values != actual_values)]
  )
  # deduplicate actual_fitted_df and add frequencies (counts)
  duplicate_df <- aggregate(
    cbind(
      actual_fitted_df[0],
      count = 1
    ),
    actual_fitted_df,
    length
  )
  # sort in descending frequency order
  duplicate_df <- duplicate_df[order(-duplicate_df$count), ]
  # compute each row count's percentage of total duplicates
  duplicate_df$percent <- round(
    x = duplicate_df$count * 100 / sum(duplicate_df$count),
    digits = digits
  )
  # compute each row's cumulative percentage
  duplicate_df$cumulative_percent <- round(
    x = ave(duplicate_df$percent, FUN = cumsum),
    digits = digits
  )
  # add column names for display in a chart
  names(duplicate_df) <- c(
    'Actual Value',
    'Fitted Value',
    'Frequency',
    '% Total',
    'Cumulative %'
  )
  # display at least one row, if one exists
  if(nrow(duplicate_df) > 0){
    index_v <- which(duplicate_df$cumulative_percent <= cutoff_cumulative_percent)
    length_index_v <- length(index_v)
    if(length_index_v == 0){
      max_index <- 1
    } else{
      max_index <- index_v[length_index_v]
    }
    duplicate_df <- duplicate_df[1:max_index, ]
  }
  # That's all folks!
  return(duplicate_df)
}

#' Dashboard generation for decision tree models
#'
#' @param config list of configuration elements
#' @param data dataframe
#' @param model model object - should be one of
#'  rpart
#'  C5.0
#' @return dashboard object for rendering
#' @export
#' @author Todd Morley
interactive_dt <- function(
  config,
  data,
  model
) {
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
  use_sampling_weights_b <- config$`Use Weights`
  n <- nrow(data)
  p <- ncol(data) - 1 - as.numeric(use_sampling_weights_b)
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
      adj_r_squared <-
        1 - (1 - r_squared) *
        (n - intercept_degrees_freedom) /
        (n - p - intercept_degrees_freedom)
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
      stop.Alteryx2(
        paste(
          'An invalid model type was passed to interactive_dt. ',
          'Please contact Alteryx support!'
        )
      )
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
    stop.Alteryx2(
      paste(
        'An invalid model type was passed to interactive_lm. ',
        'Please contact Alteryx support!'
      )
    )
  }
  if(rpart_classification_b || c50_b){
    actual_values_f <- as.factor(actual_values)
    confusion_table <- table(
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
        fdPanelRegressionScatterplot(
          actual = actual_values,
          predicted  = fitted_values
        ),
        width = totalWidth
      )
    )
    row_2_2 <- fdRow(
      fdBox(
        fdPanelRegressionMetrics(
          actual = actual_values,
          predicted  = fitted_values,
          metrics = c("MAE", "MAPE", "MedianAPE", "RMSE", "RAE", "R2_Score") # not "RMSLE"
        ),
        width = totalWidth
      )
    )
    row_2_3 <- fdRow(
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
      row_2_3,
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
    row_2_1 <- fdRow(
      fdBox(
        title = 'Misclassification Matrix',
        fdPlotMismatchMatrix(confusion_table, digits = digits),
        footer = 'This table shows misclassified pairs sorted by frequency.',
        width = totalWidth
      )
    )
    page_2 <-fdPage(
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
      body <- fdBody(
        page_1,
        page_2,
        page_3
      )

    } else{
      body <- fdBody(
        page_1,
        page_2
      )
    }
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
  }
  fdBoard(
    fdHeader(
      title = title,
      titleWidth = 600),
    fdSidebar(sidebar),
    body,
    fixed = TRUE
  )
}

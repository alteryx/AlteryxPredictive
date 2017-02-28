#' linear-model interactive-report function
#' This function generates the interactive report for the Linear Model tool.
#' @param config config
#' @param data data (breaking the usual mold a little)
#' @param model model
#' @param cv_metrics vector of CV model-performance metrics:
#'   R2, adj R2, avg MAE, avg MAPE, avg MSE, avg RMSE
#' @import MLmetrics DT
#' @export
#' @author Todd Morley, Dylan Blanchard
interactive_lm_report <- function(
  config,
  data,
  model,
  cv_metrics = NULL # expects output vector from runCrossValidationLinReg
){
  if(config$`Use Weights`) {
    data <- data[,-NCOL(data)]
  }
  requireNamespace("flightdeck")
  # UI layout constants
  totalWidth <- 12
  infoBoxWidth <- 6
  digits <- 3

  # Prep inputs.

  lm_b <- FALSE
  regularized_b <- FALSE
  cv_b <- FALSE
  if('lm' %in% class(model)){
    lm_b <- TRUE
  } else if('glmnet' %in% class(model)){
    # remove extra column in data not trained on
    data <- data[,c(model$yvar, model$xvars)]
    regularized_b <- TRUE
  } else if('cv.glmnet' %in% class(model) ){
    # remove extra column in data not trained on
    data <- data[,c(model$yvar, model$xvars)]
    cv_b <- TRUE
  } else{
    return(
      badDash(
        XMSG(
          in.targetString_sc = 'Interactive visualization not available for models of class @1.',
          in.firstBindVariable_sc = class(model)
        )
      )
    )
  }
  the_model <- model
  the_data <- data
  the_actual_values <- the_data[, 1]
  fitted_intercept <- !config$`Omit Constant`
  alpha <- config$alpha
  use_cv_lambda_1se <- config$lambda_1se
  lambda <- config$lambda_no_cv
  n <- nrow(the_data)
  p <- ncol(the_data) - 1
  # model-summary numbers
  if(lm_b){
    the_fitted_values <- unname(the_model$fitted.values)
  } else{
    independent_variable_m <- sapply(
      X = unname(the_data[, -1]),
      FUN = as.numeric,
      simplify = 'array'
    )
    if(regularized_b){
      lambda <- config$lambda_no_cv
    } else{
      if(use_cv_lambda_1se){
        lambda <- the_model$lambda.1se
      } else{
        lambda <- the_model$lambda.min
      }
    }
    the_fitted_values <- unname(
        predict(
          object = the_model,
          newx = independent_variable_m,
          s = lambda,
          type = 'response'
        )[, 1, drop = TRUE]
    )
  }
  the_residuals <- unname(the_actual_values - the_fitted_values)
  if(fitted_intercept){
    intercept_degrees_freedom <- 1
  } else{
    intercept_degrees_freedom <- 0
  }
#  We're no longer using MLMetrics for R squared.  It's numerically unstable.
#  r_squared <- R2_Score(
#    y_pred = the_fitted_values,
#    y_true = the_actual_values
#  )
  if(is.null(cv_metrics)){
    #In R, testing inherits(vec, "numeric") when vec is a vector of
    #integers returns FALSE. Thus, we need to coerce the_fitted_values
    #and the_actual_values to numeric if they're integer vectors before
    #calling rSquared, since that function tests if the input vectors
    #are numeric and errors if they're not. We don't want to change the
    #behavior of rSquared because correlation is defined on the real numbers.
    the_fitted_values_r2 <- the_fitted_values
    the_actual_values_r2 <- the_actual_values
    if (inherits(x = the_fitted_values, what = "integer")) {
      the_fitted_values_r2 <- as.numeric(the_fitted_values)
    }
    if (inherits(x = the_actual_values, what = "integer")) {
      the_actual_values_r2 <- as.numeric(the_actual_values)
    }
    r_squared <- rSquared(
      numeric_vector_1 = the_fitted_values_r2,
      numeric_vector_2 = the_actual_values_r2
    )
    adj_r_squared <- adj_r_squared(
      r_squared = r_squared,
      n = n,
      p = p,
      intercept_degrees_freedom = intercept_degrees_freedom
    )
    mae <- MAE(
      y_pred = the_fitted_values,
      y_true = the_actual_values
    )
    mape <- MAPE(
      y_pred = the_fitted_values,
      y_true = the_actual_values
    )
    mse <- MSE(
      y_pred = the_fitted_values,
      y_true = the_actual_values
    )
    rmse <- RMSE(
      y_pred = the_fitted_values,
      y_true = the_actual_values
    )
  } else{
    r_squared <- cv_metrics['r_squared']
    adj_r_squared <- cv_metrics['adj_r_squared']
    mae <- cv_metrics['avg_mae']
    mape <- cv_metrics['avg_mape']
    mse <- cv_metrics['avg_mse']
    rmse <- cv_metrics['avg_rmse']
  }
  if(lm_b){
    sigma <- sigma(the_model)
    f_statistic_text <- XMSG(
      in.targetString_sc = '@1 on @2 and @3 degrees of freedom',
      in.firstBindVariable_sc = round(
        x = summary(the_model)$fstatistic[1],
        digits = 2
      ),
      in.secondBindVariable_sc = round(
        x = summary(the_model)$fstatistic[2],
        digits = 2
      ),
      in.thirdBindVariable_sc = summary(the_model)$fstatistic[3]
    )
  }

  # Prepare UI elements.

  the_header <- fdHeader(title = XMSG(in.targetString_sc = 'Linear Regression'))

  # page 1:  summary (all models, most of it)

  row_1_1 <- fdRow(
    fdInfoBox(
      title = XMSG(in.targetString_sc = 'R Squared'),
      value = round(
        x = r_squared,
        digits = digits
      ),
      icon = fdIcon(
        name = 'check',
        lib = 'font-awesome'
      ),
      color = 'blue',
      width = infoBoxWidth
    ),
    fdInfoBox(
      title = XMSG(in.targetString_sc = 'Adjusted R Squared'),
      value = round(
        x = adj_r_squared,
        digits = digits
      ),
      icon = fdIcon(
        name = 'check',
        lib = 'font-awesome'
      ),
      color = 'blue',
      width = infoBoxWidth
    )
  )

  row_1_2 <- fdRow(
    fdInfoBox(
      title = XMSG(in.targetString_sc = 'Mean Absolute Error'),
      value = round(
        x = mae,
        digits = digits
      ),
      icon = fdIcon(
        name = 'check',
        lib = 'font-awesome'
      ),
      color = 'blue',
      width = infoBoxWidth
    ),
    fdInfoBox(
      title = XMSG(in.targetString_sc = 'Mean Absolute Percent Error'),
      value = round(
        x = mape,
        digits = digits
      ),
      icon = fdIcon(
        name = 'check',
        lib = 'font-awesome'
      ),
      color = 'blue',
      width = infoBoxWidth
    )
  )

  row_1_3 <- fdRow(
    fdInfoBox(
      title = XMSG(in.targetString_sc = 'Mean Squared Error'),
      value = round(
        x = mse,
        digits = digits
      ),
      icon = fdIcon(
        name = 'check',
        lib = 'font-awesome'
      ),
      color = 'blue',
      width = infoBoxWidth
    ),
    fdInfoBox(
      title = XMSG(in.targetString_sc = 'Root Mean Squared Error'),
      value = round(
        x = rmse,
        digits = digits
      ),
      icon = fdIcon(
        name = 'check',
        lib = 'font-awesome'
      ),
      color = 'blue',
      width = infoBoxWidth
    )
  )

  if(lm_b){
    row_1_4 <- fdRow(
      fdInfoBox(
        title = XMSG(in.targetString_sc = 'F-Statistic'),
        value = f_statistic_text,
        icon = fdIcon(
          name = 'check',
          lib = 'font-awesome'
        ),
        color = 'blue',
        width = infoBoxWidth
      ),
      fdInfoBox(
        title = XMSG(in.targetString_sc = 'Residual Standard Error'),
        value = XMSG(
          in.targetString_sc = '@1 on @2 degrees of freedom',
          in.firstBindVariable_sc = round(
            x = sigma,
            digits = digits
          ),
          in.secondBindVariable_sc = n - p
        ),
        icon = fdIcon(
          name = 'check',
          lib = 'font-awesome'
        ),
        color = 'blue',
        width = infoBoxWidth
      )
    )
  }

  row_1_5 <- fdRow(
    if(lm_b){
      fdBox(
        fdPanelCoefficients(
          mod = the_model,
          digits = digits,
          barColor = 'steelblue'
        ),
        width = totalWidth
      )
    } else{
      fdBox(
        fdPanelCoefficients(
          mod = the_model,
          digits = digits,
          barColor = 'steelblue',
          s = lambda
        ),
        width = totalWidth
      )
    }
  )

  if(lm_b){
    page_1 <- fdPage(
      row_1_1,
      row_1_2,
      row_1_3,
      row_1_4,
      row_1_5,
      id = 'page_1',
      display = TRUE
    )
  } else{
    page_1 <- fdPage(
      row_1_1,
      row_1_2,
      row_1_3,
      row_1_5,
      id = 'page_1',
      display = TRUE
    )
  }

  # page 2:  model performance (all models)

  row_2_1 <- fdRow(
    fdBox(
      fdPanelRegressionScatterplot(
        actual = the_actual_values,
        predicted  = the_fitted_values
      ),
      width = totalWidth
    )
  )

# We can't display the outer-CV metrics with this tool, and page 1 displays the same
# numbers, so this panel is out.
#  row_2_2 <- fdRow(
#    fdBox(
#      fdPanelRegressionMetrics(
#        actual = the_actual_values,
#        predicted  = the_fitted_values,
#        metrics = c("MAE", "MAPE", "MedianAPE", "RMSE", "RAE", "R2_Score") # not "RMSLE"
#      ),
#      width = totalWidth
#    )
#  )

  row_2_2 <- fdRow(
    fdBox(
      fdPanelHistogram(
        x = the_residuals,
        digits = digits,
        plotTitle = XMSG(in.targetString_sc = 'Histogram of Residuals')
      ),
      width = totalWidth
    )
  )

  page_2 <- fdPage(
    row_2_1,
    row_2_2,
#    row_2_3,
    id = 'page_2',
    display = FALSE
  )

  # Render.

  if(lm_b){
    the_title <- XMSG(in.targetString_sc = 'Ordinary Least-Squares Linear Regression')
    # page 3: lm diagnostics
    row_3_1 <- fdRow(
      fdBox(
        fdPanelRegressionDiagnostics(mod = the_model),
        width = totalWidth
      )
    )
    page_3 <- fdPage(
      row_3_1,
      id = 'page_3',
      display = FALSE
    )
    the_body <- fdBody(
      page_1,
      page_2,
      page_3
    )
    third_menu_item <- fdMenuItem(
      text = XMSG(in.targetString_sc = 'Diagnostics'),
      icon = fdIcon(
        name = 'caret-right',
        lib = "font-awesome"
      ),
      pageName = 'page_3'
    )
  }

  if(regularized_b){
    the_title <- XMSG(in.targetString_sc = 'Elastic-Net Linear Regression')
    # page 4:  glmnet coefficient profile paths
    row_4_1 <- fdRow(
      fdBox(
        fdPlotGlmnet(
          x = the_model,
          xvar = 'norm',
          title = XMSG(in.targetString_sc = 'Coefficients vs. L1 Norm')
        ),
        width = totalWidth
      )
    )
    row_4_2 <- fdRow(
      fdBox(
        fdPlotGlmnet(
          x = the_model,
          xvar = 'lambda',
          title = XMSG(in.targetString_sc = 'Coefficients vs. Log(Lambda)')
        ),
        width = totalWidth
      )
    )
    row_4_3 <- fdRow(
      fdBox(
        fdPlotGlmnet(
          x = the_model,
          xvar = 'dev',
          title = XMSG(in.targetString_sc = 'Coefficients vs. Percent Deviance Explained')
        ),
        width = totalWidth
      )
    )
    page_4 <- fdPage(
      row_4_1,
      row_4_2,
      row_4_3,
      id = 'page_4',
      display = FALSE
    )
    the_body <- fdBody(
      page_1,
      page_2,
      page_4
    )
    third_menu_item <- fdMenuItem(
      text = XMSG(in.targetString_sc = 'Coefficient Profiles'),
      icon = fdIcon(
        name = 'caret-right',
        lib = "font-awesome"
      ),
      pageName = 'page_4'
    )
  }

  if(cv_b) {
    the_title <- XMSG(in.targetString_sc = 'Cross-Validated Elastic-Net Linear Regression')
    # page 5:  c.vglmnet CV MSE vs. log(lambda)
    row_5_1 <- fdRow(
      fdBox(
        fdPlotCvGlmnet(
          x = the_model,
          sign.lambda = NULL
        ),
        width = totalWidth
      )
    )
    page_5 <- fdPage(
      row_5_1,
      id = 'page_5',
      display = FALSE
    )
    the_body <- fdBody(
      page_1,
      page_2,
      page_5
    )
    third_menu_item <- fdMenuItem(
      text = XMSG(in.targetString_sc = 'Lambda Tuning'),
      icon = fdIcon(
        name = 'caret-right',
        lib = "font-awesome"
      ),
      pageName = 'page_5'
    )
  }

  the_sidebar <- fdSidebarMenu(
    fdMenuItem(
      text = XMSG(in.targetString_sc = 'Summary'),
      icon = fdIcon(
        name = 'caret-right',
        lib = "font-awesome"
      ),
      pageName = 'page_1'
    ),
    fdMenuItem(
      text = XMSG(in.targetString_sc = 'Model Performance'),
      icon = fdIcon(
        name = 'caret-right',
        lib = "font-awesome"
      ),
      pageName = 'page_2'
    ),
    third_menu_item
  )
  titleWidth <- computeWidth(the_title)
  fdBoard(
    fdHeader(
      title = the_title,
      titleWidth = titleWidth
    ),
    fdSidebar(
      the_sidebar,
      sidebarWidth = titleWidth
    ),
    the_body,
    fixed = TRUE
  )
}

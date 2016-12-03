#' Create a data frame with lm model object summary
#'
#'
#' The function Alteryx.ReportLM creates a data frame of an lm model's summary
#' output that can more easily be handled by Alteryx's reporting tools. The
#' function returns a data frame that is a set of key-value pairs for the
#' components of the summary report as well as the model's coefficients in a
#' JSON formatted character string.
#'
#' @param lm.obj lm model object whose summary output is put into a data frame
#' @author Dan Putler
#' @export
#' @family Alteryx.Report
Alteryx.ReportLM <- function (lm.obj){
  if (class(lm.obj) != "lm")
    stop.Alteryx2("The object provided is not a glm class object")
  full.sum <- summary(lm.obj)
  the.call <- paste(capture.output(full.sum$call), collapse = "")
  the.call = gsub("\\s\\s", "", the.call)
  f <- formula(lm.obj)
  the.call <- sub("formula = ([^\\,]+)",
    paste("formula =", paste(f[2], "~", f[3])), the.call)
  resid.sum <- paste(format(summary(full.sum$residuals)[-4] , digits = 3), collapse = " ")
  p.stars <- pStars(full.sum$coefficients[,4])
  coef.est <- paste(dimnames(full.sum$coefficients)[[1]], format(full.sum$coefficients[,1], digits = 4),
                    format(full.sum$coefficients[,2], digits = 4), format(full.sum$coefficients[,3], digits = 4),
                    as.character(p.stars$p_txt), as.character(p.stars$Stars), sep = "|")
  coef.lab <- "Coefficients:"
  # Address variables that were omitted due to singularities
  omitted <- names(full.sum$aliased)[full.sum$aliased]
  if (length(omitted) > 0) {
    coef.lab <- paste(coef.lab, " (", length(omitted), " not defined because of singularities)", sep = "")
    for (i in omitted)
      coef.est <- c(coef.est, paste(i, "NA|NA|NA|NA|  ", sep = "|"))
  }
  resid.se <- paste("Residual standard error:", format(full.sum$sigma, digits = 5), "on", full.sum$df[2], "degrees of freedom")
  r.sq <- paste("Multiple R-squared: ", format(full.sum$r.squared, digits = 4), ", Adjusted R-Squared: ", format(full.sum$adj.r.squared, digits = 4), sep = "")
  p.f1 <- 1 - pf(full.sum$fstatistic[1], full.sum$fstatistic[2], full.sum$fstatistic[3])
  p.f2 <- format(p.f1, digits = 4)
  p.f2[p.f1 < 2.2e-16] <- "< 2.2e-16"
  f.stat <- paste("F-statistic: ", format(full.sum$fstatistic[1], digits = 4), " on ", as.integer(full.sum$fstatistic[2]), " and ", as.integer(full.sum$fstatistic[3]), " DF, p-value: ", p.f2, sep = "")
  sum.grps <- c("Call", "Residuals", "Coef_Label", rep("Coef_Est", length(coef.est)), rep("Fit_Stats", 3))
  sum.out <- c(the.call, resid.sum, coef.lab, coef.est, resid.se, r.sq, f.stat)
  summary.df <- data.frame(grp = sum.grps, out = sum.out)
  summary.df$grp <- as.character(summary.df$grp)
  summary.df$out <- as.character(summary.df$out)
  json.str <- paste("\"", names(lm.obj$coefficients), "\":\"",
                    lm.obj$coefficients, "\"", sep = "", collapse = ", ")
  json.str <- paste("{", json.str, "}")
  coef.str <- c("Coef_JSON", json.str)
  summary.df <- rbind(summary.df, coef.str)
  summary.df
}


#' Create a data frame with glm model object summary
#'
#'
#' The function Alteryx.ReportGLM creates a data frame of an glm model's summary
#' output that can more easily be handled by Alteryx's reporting tools. The
#' function returns a data frame that is a set of key-value pairs for the
#' components of the summary report as well as the model's coefficients in a
#' JSON formatted character string. The coefficient JSON string is really for
#' future option value
#'
#' @param glm.obj glm model whose summary output is put into a data frame
#' @author Dan Putler
#' @export
#' @family Alteryx.Report
Alteryx.ReportGLM <- function (glm.obj){
  if (class(glm.obj)[1] != "glm" && class(glm.obj)[2] != "glm")
    stop.Alteryx2("The object provided is not a glm class object")
  full.sum <- summary(glm.obj)
  the.call <- paste(capture.output(full.sum$call), collapse = "")
  the.call = gsub("\\s\\s", "", the.call)
  f <- formula(glm.obj)
  the.call <- sub("formula = ([^\\,]+)",
    paste("formula =", paste(f[2], "~", f[3])), the.call)
  resid.sum <- paste(format(summary(full.sum$deviance.resid)[-4], digits = 3), collapse = " ")
  p.stars <- pStars(full.sum$coefficients[,4])
  coef.est <- paste(dimnames(full.sum$coefficients)[[1]], format(full.sum$coefficients[,1], digits = 4),
                    format(full.sum$coefficients[,2], digits = 4), format(full.sum$coefficients[,3], digits = 4),
                    as.character(p.stars$p_txt), as.character(p.stars$Stars), sep = "|")
  coef.lab <- "Coefficients:"
  # Address variables that were omitted due to singularities
  omitted <- names(full.sum$aliased)[full.sum$aliased]
  if (length(omitted) > 0) {
    coef.lab <- paste(coef.lab, " (", length(omitted), " not defined because of singularities)", sep = "")
    for (i in omitted)
      coef.est <- c(coef.est, paste(i, "NA|NA|NA|NA|  ", sep = "|"))
  }
  # If there is a theta and SE.theta elements of the object it is glm.nb model
  # and add the estimates of theta and its se to ceof.est
  if (!is.null(glm.obj$theta))
    coef.est <- c(coef.est, paste("theta", format(glm.obj$theta, digits = 6), format(glm.obj$SE.theta, digits = 6), "| | ", sep = "|"))
  dispersion <- paste("(Dispersion parameter for ", full.sum$family$family, " taken to be ", full.sum$dispersion, ")", sep = "")
  df.null <- full.sum$df[2] + full.sum$df[3] - 1
  null.dev <- paste("Null deviance:", format(full.sum$null.deviance, digits = 5), "on", df.null, "degrees of freedom")
  mod.dev <- paste("Residual deviance:", format(full.sum$deviance, digits = 5), "on", full.sum$df[2], "degrees of freedom")
  McF.R2 <- 1 - (glm.obj$deviance/glm.obj$null.deviance)
  mod.fit <- paste("McFadden R-Squared: ", format(McF.R2, digits = 4), ", AIC: ", format(full.sum$aic, digits = 4), sep="")
  fisher.it <- paste("Number of Fisher Scoring iterations:", full.sum$iter)
  sum.grps <- c("Call", "Residuals", "Coef_Label", rep("Coef_Est", length(coef.est)), "Dispersion", rep("Fit_Stats", 3), "Fisher")
  sum.out <- c(the.call, resid.sum, coef.lab, coef.est, dispersion, null.dev, mod.dev, mod.fit, fisher.it)
  summary.df <- data.frame(grp = sum.grps, out = sum.out)
  summary.df$grp <- as.character(summary.df$grp)
  summary.df$out <- as.character(summary.df$out)
  json.str <- paste("\"", names(glm.obj$coefficients), "\":\"",
    glm.obj$coefficients, "\"", sep = "", collapse = ", "
  )
  json.str <- paste("{", json.str, "}")
  coef.str <- c("Coef_JSON", json.str)
  summary.df <- rbind(summary.df, coef.str)
  singular <- length(omitted) > 0
  list(summary.df = summary.df, singular = singular)
}

#' Create a data frame with model object summary
#'
#'
#' The function Alteryx.ReportAnova creates a data frame of key-value pairs for
#' the purpose of assisting Alteryx's reporting tools to report the results of a
#' type II ANOVA for a LM or GLM model object. The last key- value indicates
#' whether the report is for a lm or glm model which is relevant for labeling
#' columns in the output created by Alteryx.
#'
#' @param model.obj model object whose summary output is put into a data frame
#' @author Dan Putler
#' @export
#' @family Alteryx.Report
Alteryx.ReportAnova <- function (model.obj)
{
  if (class(model.obj)[1] != "lm" && class(model.obj)[1] !=
      "glm" && class(model.obj)[2] != "glm") {
    stop.Alteryx2("The object provided is not a lm or glm class object")
  }
  the.anova <- car::Anova(model.obj, type = "II")
  response <- attributes(the.anova)$heading[2]
  if (class(model.obj)[1] == "lm") {
    p.vals <- the.anova[[4]]
    p.vals <- p.vals[1:(length(p.vals) - 1)]
    p.stars <- pStars(p.vals)
    f.vals1 <- the.anova[[3]]
    f.vals <- as.character(round(f.vals1[1:(length(f.vals1) - 1)], 2))
    the.table <- paste(attributes(the.anova)$row.names, round(the.anova[[1]], 2),
      round(the.anova[[2]], 0), c(f.vals,""), c(as.character(p.stars$p_txt), ""),
      c(as.character(p.stars$Stars), ""),	sep = "|"
    )
  } else {
    p.stars <- pStars(the.anova[[3]])
    the.table <- paste(attributes(the.anova)$row.names, round(the.anova[[1]], 3),
      round(the.anova[[2]], 0), as.character(p.stars$p_txt),
      as.character(p.stars$Stars), sep = "|"
    )
  }
  anova_grps <- c("Anova_Resp", rep("Anova_Test", length(the.table)))
  anova.df <- data.frame(grp = anova_grps, out = c(response, the.table))
  anova.df$grp <- as.character(anova.df$grp)
  anova.df$out <- as.character(anova.df$out)
  anova.df <- rbind(anova.df, c("Model_Class", class(model.obj)[1]))
  return(anova.df)
}


# The function Alteryx.ParseCoefSum parses and formats the lines from the
# coefficient summary portion of an lm or glm model object. The output is
# placed into pipe (|) delimited fields for further processing within an
# Alteryx macro
# Author: Dan Putler
Alteryx.ParseCoefSum <- function(coef_sum) {
  if(!is.character(coef_sum)) {
    stop.Alteryx2("The argument to the function must be a character vector")
  }
  # parseRow is the function that is used in the apply function
  parseRow <- function(a_row) {
    cs_vec <- unlist(strsplit(a_row, "\\s"))
    # Get the significant level indicator
    cs_stars <- cs_vec[length(cs_vec)]
    # If there is only a single star or a period, then there is a last space
    if(cs_vec[(length(cs_vec) - 1)] == "*" | cs_vec[(length(cs_vec) - 1)] == ".") {
      cs_stars = cs_vec[(length(cs_vec) - 1)]
    }
    cs_stars[cs_stars == ""] <- " "
    if(cs_stars == "*" | cs_stars == ".") {
      cs_vec <- cs_vec[1:(length(cs_vec) - 2)]
    } else {cs_vec <- cs_vec[1:(length(cs_vec) - 1)]}
    cs_vec <- cs_vec[cs_vec != ""]
    # Deal with the "bottom-ending" of the p-values
    if(cs_vec[(length(cs_vec) - 1)] == "<") { # limit p-value
      pval <- paste(cs_vec[(length(cs_vec) - 1)], cs_vec[length(cs_vec)])
    } else {pval <- cs_vec[length(cs_vec)]}
    # Deal with variable indicators that have embedded spaces
    var_name <- cs_vec[1]
    i <- 2
    repeat{
      print(cs_vec[i])
      if(is.na(as.numeric(cs_vec[i])) & cs_vec[i] != "NA") {
        var_name <- paste(var_name, cs_vec[i])
        i <- i + 1
      } else {break}
    }
    # Construct the parsed, pipe-delimited line
    out_vec <- paste(var_name, cs_vec[i], cs_vec[(i + 1)], cs_vec[(i + 2)],
                     pval, cs_stars, sep="|")
    return(out_vec)
  }
  # Apply the function
  parsed_rows <- sapply(coef_sum, parseRow)
  names(parsed_rows) <- NULL
  return(parsed_rows)
}


# The function Alteryx.ParseAnova parses and formats the lines from Anova
# Type II results for an lm or glm model object. The output is placed into
# pipe (|) delimited fields for further processing within an Alteryx macro
# Author: Dan Putler
Alteryx.ParseAnova <- function(the_anova, obj.class) {
  if(!is.character(the_anova)) {
    stop.Alteryx2("The argument to the function must be a character vector")
  }
  # parseRow is the function that is used in the apply function
  parseRow <- function(a_row, obj.class) {
    anova_vec <- unlist(strsplit(a_row, "\\s"))
    anova_vec <- anova_vec[anova_vec != ""]
    if(obj.class == "lm") {
      # Deal with the "bottom-end" of the reported p-values
      if(anova_vec[5] == "<" & !is.na(anova_vec[5])) {
        anova_vec <- c(anova_vec[1:4], paste(anova_vec[5], anova_vec[6]),
                       anova_vec[7])
      }
      # Deal with the final row of the table for an lm object
      if(is.na(anova_vec[4])) anova_vec <- c(anova_vec, rep(" ", 3))
      # Deal with reported significance for insignificant tests
      if(is.na(anova_vec[6])) anova_vec <- c(anova_vec, " ")
      # Create the reformatted rows
      anova_out <- paste(anova_vec[1], anova_vec[2], anova_vec[3], anova_vec[4],
                         anova_vec[5], anova_vec[6], sep="|")
    } else { # glm class objects
      # Deal with the "bottom-end" of the reported p-values
      if(anova_vec[4] == "<" & !is.na(anova_vec[4])) {
        anova_vec <- c(anova_vec[1:3], paste(anova_vec[4], anova_vec[5]),
                       anova_vec[6])
      }
      # Deal with reported significance for insignificant tests
      if(is.na(anova_vec[5])) anova_vec <- c(anova_vec, " ")
      # Create the reformatted rows
      anova_out <- paste(anova_vec[1], anova_vec[2], anova_vec[3], anova_vec[4],
                         anova_vec[5], sep="|")
    }
    return(anova_out)
  }
  # Apply the function
  parsed_rows <- sapply(the_anova, parseRow, obj.class)
  names(parsed_rows) <- NULL
  return(parsed_rows)
}

#' Create report for rx model objects.
#'
#' @param rx.obj model object of class rxLinMod, rxLogit or rxGlm
#' @param null.deviance null deviance
#' @export
AlteryxReportRx <- function (rx.obj, null.deviance = NULL) {
  if (!(class(rx.obj) %in% c("rxLinMod","rxLogit","rxGlm")))
    stop.Alteryx2("The object provided is not an appropriate RevoScaleR class object")
  the.call <- paste(capture.output(rx.obj$call), collapse = "")
  the.call = gsub("\\s\\s", "", the.call)
  # The coefficients and related estimates need to be done by class
  if (class(rx.obj) == "rxLinMod") {
    param.names <- attributes(rx.obj$coefficients)$dimnames[[1]]
    coefs1 <- rx.obj$coefficients[,1]
    the.coefs <- format(coefs1, digits = 4)
    the.coefs[is.na(coefs1)] <- "Dropped"
    the.se <- format(rx.obj$coef.std.error[,1], digits = 4)
    the.se[is.na(coefs1)] <- "Dropped"
    the.t <- format(rx.obj$coef.t.value[,1], digits = 4)
    the.t[is.na(coefs1)] <- "Dropped"
    p.stars <- pStars(rx.obj$coef.p.value[,1])
    p.stars$p_txt <- as.character(p.stars$p_txt)
    p.stars$p_txt[is.na(coefs1)] <- "Dropped"
    p.stars$Stars <- as.character(p.stars$Stars)
    p.stars$Stars[is.na(coefs1)] <- " "
  } else {
    param.names <- names(rx.obj$coefficients)
    the.coefs <- format(rx.obj$coefficients, digits = 4)
    the.coefs[is.na(rx.obj$coefficients)] <- "Dropped"
    the.se <- format(rx.obj$coef.std.error, digits = 4)
    the.se[is.na(rx.obj$coefficients)] <- "Dropped"
    the.t <- format(rx.obj$coef.t.value, digits = 4)
    the.t[is.na(rx.obj$coefficients)] <- "Dropped"
    p.stars <- pStars(rx.obj$coef.p.value)
    p.stars$p_txt <- as.character(p.stars$p_txt)
    p.stars$p_txt[is.na(rx.obj$coefficients)] <- "Dropped"
    p.stars$Stars <- as.character(p.stars$Stars)
    p.stars$Stars[is.na(rx.obj$coefficients)] <- " "
  }
  coef.est <- paste(param.names, the.coefs, the.se, the.t, p.stars$p_txt, p.stars$Stars, sep = "|")
  coef.lab <- "Coefficients:"
  omitted <- names(rx.obj$aliased)[rx.obj$aliased]
  if (length(omitted) > 0)
    coef.lab <- paste(coef.lab, " (", length(omitted), " not defined because of singularities)", sep = "")
  # Model summary, slightly different for glm based objects versus lm objects
  if (class(rx.obj) != "rxLinMod") {
    if (class(rx.obj) == "rxGlm")
      dispersion <- paste("(Dispersion parameter for ", rx.obj$family$family, " taken to be ", rx.obj$dispersion, ")", sep = "")
    if (class(rx.obj) == "rxLogit")
      dispersion <- "(Dispersion parameter for binomial taken to be 1)"
    df.null <- rx.obj$nValidObs - 1
    df.mod <- rx.obj$nValidObs - length(param.names)
    null.dev <- paste("Null deviance:", format(null.deviance, digits = 5), "on", df.null, "degrees of freedom")
    mod.dev <- paste("Residual deviance:", format(rx.obj$deviance, digits = 5), "on", df.mod, "degrees of freedom")
    McF.R2 <- 1 - (rx.obj$deviance/null.deviance)
    mod.fit <- paste("McFadden R-Squared: ", format(McF.R2, digits = 4), ", AIC: ", format(rx.obj$aic, digits = 4), sep = "")
    fisher.it <- paste("Number of IRLS iterations:", rx.obj$iter)
    sum.grps <- c("Call", "Coef_Label", rep("Coef_Est", length(coef.est)), "Dispersion", rep("Fit_Stats", 3), "Fisher")
    sum.out <- c(the.call, coef.lab, coef.est, dispersion, null.dev, mod.dev, mod.fit, fisher.it)
    summary.df <- data.frame(grp = sum.grps, out = sum.out)
    summary.df$grp <- as.character(summary.df$grp)
    summary.df$out <- as.character(summary.df$out)
  } else {
    resid.se <- paste("Residual standard error:", format(rx.obj$sigma, digits = 5), "on", rx.obj$df[2], "degrees of freedom")
    r.sq <- paste("Multiple R-squared: ", format(rx.obj$r.squared, digits = 4), ", Adjusted R-Squared: ", format(rx.obj$adj.r.squared, digits = 4), sep = "")
    p.f <- format(rx.obj$f.pvalue, digits = 4)
    p.f[rx.obj$f.pvalue < 2.2e-16] <- "< 2.2e-16"
    f.stat <- paste("F-statistic: ", format(rx.obj$fstatistic$value, digits = 4), " on ", as.integer(rx.obj$fstatistic$numdf), " and ", as.integer(rx.obj$fstatistic$dendf), " DF, p-value: ", p.f, sep = "")
    sum.grps <- c("Call", "Coef_Label", rep("Coef_Est", length(coef.est)), rep("Fit_Stats", 3))
    sum.out <- c(the.call, coef.lab, coef.est, resid.se, r.sq, f.stat)
    summary.df <- data.frame(grp = sum.grps, out = sum.out)
    summary.df$grp <- as.character(summary.df$grp)
    summary.df$out <- as.character(summary.df$out)
  }
  json.str <- paste("\"", names(rx.obj$coefficients), "\":\"", rx.obj$coefficients, "\"", sep = "", collapse = ", ")
  json.str <- paste("{", json.str, "}")
  coef.str <- c("Coef_JSON", json.str)
  summary.df <- rbind(summary.df, coef.str)
  summary.df
}


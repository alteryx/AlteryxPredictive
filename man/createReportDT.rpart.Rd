% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decisiontreeutils.R
\name{createReportDT.rpart}
\alias{createReportDT.rpart}
\title{Get data for static report (grp|out pipes) for rpart model}
\usage{
\method{createReportDT}{rpart}(model, config, names, xdf_path)
}
\arguments{
\item{model}{model object}

\item{config}{list of config options}

\item{names}{names of variables (x, y and w)}

\item{xdf_path}{string of xdf file location}
}
\value{
dataframe of piped results
}
\description{
Get data for static report (grp|out pipes) for rpart model
}

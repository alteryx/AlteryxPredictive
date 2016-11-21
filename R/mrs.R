#' Get MRS levels
#'
#' Get the names of potential factors in a table
#' Note 1: Applying names() to the returned mrsLevels objects, the names of the
#' factor variable can be determined.
#' Note 2: The set of factor variables altered in subsequent steps can be
#' addressed by using an approach such as
#' mrs_levels[names(mrs_levels) %in% desired_factor_names]
#'
#' @param data  data
#' @param loc_tmp loc_tmp
#' @param rem_tmp rem_tmp
#' @param rem_R rem_R
#' @export
#' @author Dan Putler
mrsLevels <- function(data, loc_tmp, rem_tmp = NULL, rem_R = NULL) {
  con_string <- data@connectionString
  context <- unlist(strsplit(unlist(strsplit(con_string, ";"))[1], "DRIVER="))[2]
  if (context == "Teradata") {
    if(is.null(rem_tmp) || is.null(rem_R)) {
      stop.Alteryx2("Not all of the information about the Teradata platform has not be provided.")
    }
    cc <- RevoScaleR::RxInTeradata(connectionString = con_string, shareDir = loc_tmp, remoteShareDir = rem_tmp, revoPath = rem_R, wait = TRUE)
  } else {
    cc <-  RevoScaleR::RxInSqlServer(connectionString = con_string, shareDir = loc_tmp, wait = TRUE)
  }
  RevoScaleR::rxSetComputeContext(cc)
  all_info <-  RevoScaleR::rxGetVarInfo(data = data, computeInfo = TRUE)
  RevoScaleR::rxSetComputeContext("local")
  the_levels <- noNullLevels(lapply(all_info, function(x) x$levels))
  class(the_levels) <- "mrsLevels"
  the_levels
}

#' Reorder MRS Labels
#'
#' Take the original factor levels in a mrsLevels object and reorder them
#' using an alpha sort on the level labels.
#'
#' @param lev_obj mrs levels object
#' @export
#' @author Dan Putler
mrsReorderedLevels <- function(lev_obj) {
  if (class(lev_obj) != "mrsLevels")
    stop.Alteryx2("The provided argument is not a mrsLevels class object")
  lapply(lev_obj, function(x) x <- x[order(x)])
}

#' Create the appropriate information for the \code{colInfo} arguments of
#' \code{RxSqlServerData} and \code{RxTeradata} to get factor
#' levels in alpha order
#'
#' @inheritParams mrsLevels
#' @export
#' @author Dan Putler
mrsReorderFactors <- function(data, loc_tmp, rem_tmp, rem_R) {
  the_levels <- mrsReorderedLevels(mrsLevels(data, loc_tmp, rem_tmp, rem_R))
  col_info <- list()
  for (i in 1:length(the_levels)) {
    these_levels <- the_levels[[i]]
    this_line <- paste0('col_info$', names(the_levels)[i], ' <- list(type = "factor", levels = these_levels, newLevels = these_levels)')
    eval(parse(text = this_line))
  }
  col_info
}

#' Create MRS data object
#'
#' Take the variables to be used in an analysis along with information about the
#' table and the platform (SQL Server or Teradata) and then creates a data source
#' object with alpha sorted factor levels.
#'
#' @param con_string  connection string
#' @param table table
#' @param fields fields
#' @param loc_tmp loc_tmp
#' @param rem_tmp rem_tmp
#' @param rem_R rem_R
#' @export
#' @author Dan Putler
mrsDataObj <- function(con_string, table, fields, loc_tmp, rem_tmp, rem_R) {
  context <- unlist(strsplit(unlist(strsplit(con_string, ";"))[1], "DRIVER="))[2]
  if (context == "Teradata") {
    data_obj <-  RevoScaleR::RxTeradata(table = table, connectionString = con_string, stringsAsFactors = TRUE)
    col_info <- mrsReorderFactors(data_obj, loc_tmp, rem_tmp, rem_R)
    col_info <- col_info[names(col_info) %in% fields]
    data_obj <-  RevoScaleR::RxTeradata(table = table, connectionString = con_string, colInfo = col_info)
  } else { # SQL Server for now
    data_obj <-  RevoScaleR::RxSqlServerData(table = table, connectionString = con_string, stringsAsFactors = TRUE)
    col_info <- mrsReorderFactors(data_obj, loc_tmp, rem_tmp, rem_R)
    col_info <- col_info[names(col_info) %in% fields]
    data_obj <-  RevoScaleR::RxSqlServerData(table = table, connectionString = con_string, colInfo = col_info)
  }
  data_obj
}

#' Get MRS levels.
#'
#' Determine the levels of any predictor variables that are factors based on
#' information in an object created by mrsDataObj
#' @param data  data
#' @param fields fields
#' @export
#' @author Dan Putler
mrsGetXYLevels <- function(data, fields) {
  col_info <- data@colInfo
  out_list <- list()
  for (i in names(col_info)) {
    if (i %in% fields) {
      this_type <- eval(parse(text = paste0("col_info$", i, "$type")))
      if (this_type == "factor") {
        these_levels <- eval(parse(text = paste0("col_info$", i, "$newLevels")))
        eval(parse(text = paste0("out_list$", i, " <- these_levels")))
      }
    }
  }
  if(length(out_list) == 0) {
    out_list <- NULL
  }
  out_list
}

#' Create a temp directory for MRS related work
#'
#' \code{mrsTemp} determines if an appropriate temporary directory for MRS related
#' work is present on the user's machine, creates one if not, and returns the
#' path to the directory
#'
#' @export
#' @author Dan Putler
mrsTemp <- function() {
  alteryx_temp <- getwd()
  dir_parts <- unlist(strsplit(alteryx_temp, "/"))
  temp_root <- paste(dir_parts[1:(length(dir_parts)-1)], collapse = "/")
  mrs_temp <- paste0(temp_root, "/mrs_temp")
  if (!dir.exists(mrs_temp)) {
    dir.create(mrs_temp, recursive = TRUE)
  }
  mrs_temp
}

#' Correct MRS levels.
#'
#' Examine the levels of relevant factors in new data and make
#' them conform to the levels of the relevant factors on which the model is
#' based. This means placing unknown levels after known levels for factors that
#' have unknown levels in the new data, or adding missing factor levels and
#' sorting the levels for factors that have levels that are not present in the
#' new data
#'
#' @param e.lev existing data levels
#' @param nd.lev new data levels
#' @export
#' @author Dan Putler
mrsCorrectLevels <- function(e.lev, nd.lev) {
  out_list <- list()
  for (i in names(e.lev)) {
    # the levels
    e_levels <- e.lev[[i]]
    nd_levels <- nd.lev[[i]]
    # The case where their are levels in the estimation data that are not in the new data.
    # This covers the sub-case where the estimation data has both missing and new levels
    if (!all(names(nd_levels) %in% names(e_levels))) {
      # the levels missing in the estimation data
      m_levels <- nd_levels[!(nd_levels %in% e_levels)]
      # The revised levels (sort order does not matter for new levels)
      nd_levels <- c(e_levels, m_levels)
    } else { # The case where all the levels in the estimation data are in the new data
      if (length(nd_levels) == length(e_levels)) { # the subcase of identical levels
        nd_levels <- e_levels
      } else { # The subcase where the new data has levels not in the estimation data
        m_levels <- nd_levels[!(nd_levels %in% e_levels)]
        nd_levels <- c(e_levels, m_levels)
      }
    }
    eval(parse(text = paste0("out_list$", i, " <- nd_levels")))
  }
  out_list
}

#' Extract column info from MRS levels.
#'
#' Take the xlevels element of a predictive model object and convert into a
#' structrue that is correct as the \code{colInfo} argument to a MRS
#' in-platform data object
#'
#' @param levels x levels of MRS data object
#' @export
#' @author Dan Putler
mrsLevels2ColInfo <- function(levels) {
  col_info <- list()
  for (i in names(levels)) {
    these_levels <- levels[[i]]
    this_line <- paste0('col_info$', i, ' <- list(type = "factor", levels = these_levels, newLevels = these_levels)')
    eval(parse(text = this_line))
  }
  col_info
}

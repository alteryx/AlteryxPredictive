#' check whether some number is an integerValue
#'
#' @param x numeric to attempt to coerce to whole number
#' @param precision precision to compare to nearest whole number
is.integerValue <- function(x, precision = .Machine$double.eps^.5) {
  abs(x - round(x)) < precision
}

#' analog of assert_that for use with Alteryx
#' Generic S3 class on message
#' message can have class 'error' or 'warning'
#'
#' @param exp expression to evaluate
#' @param msg message to pass to user
#' @param ... additional arguments
Alteryx_assert <- function(exp, msg, ...) {
  UseMethod("Alteryx_assert", msg)
}

#' analog of assert_that for use with Alteryx warnings
#'
#' @inheritParams Alteryx_assert
Alteryx_assert.warning <- function(exp, msg, ...) {
  if(!exp)
    AlteryxMessage2(msg, ...)
}

#' analog of assert_that for use with Alteryx errors
#'
#' @inheritParams Alteryx_assert
Alteryx_assert.error <- function(exp, msg, ...) {
  if(!exp)
    stop.Alteryx2(msg, ...)
}

#' analog of assert_that for use with Alteryx,
#' defaults to error messaging
#'
#' @inheritParams Alteryx_assert
Alteryx_assert.default <- function(exp, msg, ...) {
  Alteryx_assert.error(exp, msg, ...)
}

#' checks whether real value is between min and max
#'
#' @param val value to check
#' @param min lower bound
#' @param max upper bound
#' @param closed logical or length-2 logical vector whether range is closed
is.boundedReal <- function(val, min = -Inf, max = Inf, closed = TRUE) {
  closed <- if (length(closed) == 1) rep(closed, 2) else closed[1:2]
  is.numeric(val) && min <= val && max >= val &&
    (!closed[1] || min != val) && (!closed[2] || max != val)
}

#' checks whether real value is between min and max
#'
#' @inheritParams is.boundedReal
is.boundedInt <- function(val, min = -Inf, max = Inf, closed = TRUE) {
  is.integerValue(val) && is.boundedReal(val = val, min = min, max = max, closed = closed)
}

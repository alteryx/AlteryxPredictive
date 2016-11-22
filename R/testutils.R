#' check whether some number is whole number
#'
#' @param x numeric to attempt to coerce to whole number
#' @param precision precision to compare to nearest whole number
is.wholenumber <- function(x, precision = .Machine$double.eps^.5) {
  abs(x - round(x)) < precision
}

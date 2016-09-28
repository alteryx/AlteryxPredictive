# TODO: Test more scenarios. Consider edge cases carefully.

context("utils")
test_that("makeFormula works", {
  xvars <- c('x1', 'x2', 'x3')
  yvar <- 'y'
  f <- makeFormula(xvars, yvar)
  e <- formula(y ~ x1 + x2 + x3)
  expect_equal(f, e)
})

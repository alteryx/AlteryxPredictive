context("makeFormula testing")

test_that("makeFormula works", {
  xvars <- c('x1', 'x2', 'x3')
  yvar <- 'y'
  f <- makeFormula(xvars, yvar)
  e <- formula(y ~ x1 + x2 + x3)
  expect_equal(f, e)
})

test_that("makeFormula throws errors for invalid input", {
  expect_that(makeFormula(NULL, "y"), throws_error())
  expect_that(makeFormula(c("x1", "x2"), FALSE), throws_error())
})

context("checkValidConfig testing")

test_that("proper warning is returned for (and only for) < 5 numeric target vals", {
  is_XDF = FALSE
  the.data4 <- data.frame(y = rep(1:4, 100))
  the.data5 <- data.frame(y = rep(1:5, 100))
  names <- list(y = "y")
  config <- list(cp = .01)
  expect_that(checkValidConfig(config, the.data4, names, is_XDF),
              gives_warning("The target variable is numeric, however, it has 4 or fewer unique values."))
  expect_that(checkValidConfig(config, the.data5, names, is_XDF),
              gives_warning(NA))
})

test_that("proper error throwing is done for invalid control parameter", {
  is_XDF = FALSE
  the.data <- data.frame(y = 1:100)
  names <- list(y = "y")
  config0 <- list(cp = 0)
  config1 <- list(cp = 1)
  configblah <- list(cp = "blah")
  expect_that(checkValidConfig(config0, the.data, names, is_XDF),
              throws_error("The complexity parameter must be between 0 and 1. Please try again."))
  expect_that(checkValidConfig(config1, the.data, names, is_XDF),
              throws_error("The complexity parameter must be between 0 and 1. Please try again."))
  expect_that(checkValidConfig(configblah, the.data, names, is_XDF),
              throws_error("The complexity parameter provided is not a number. Please enter a new value and try again."))
})

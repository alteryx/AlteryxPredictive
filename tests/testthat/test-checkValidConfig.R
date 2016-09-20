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

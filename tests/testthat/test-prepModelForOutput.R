context("prepModelForOutput testing")

test_that("prepModelForOutput works", {
  expect_equivalent(
    prepModelForOutput("name", 0),
    list(Name = "name", Object = list(0))
  )
})

test_that("prepModelForOutput reject invalid type for name", {
  expect_error(prepModelForOutput(5, 0))
})

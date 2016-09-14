context("prepModelForOutput testing")

test_that("prepModelForOutput works", {
  expect_that(prepModelForOutput("name", 0), is_equivalent_to(
    list(Name = "name", Object = list(0))
  ))
})

test_that("prepModelForOutput reject invalid type for name", {
  expect_that(prepModelForOutput(5, 0), throws_error())
})

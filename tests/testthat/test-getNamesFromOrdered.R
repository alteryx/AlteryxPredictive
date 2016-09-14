context("getNamesFromOrdered testing")

test_that("getNamesFromOrdered throws errors when list of names is too small.", {
  expect_that(getNamesFromOrdered(FALSE, letters[1:1]), throws_error())
  expect_that(getNamesFromOrdered(TRUE, letters[1:2]), throws_error())
})

test_that("getNamesFromOrdered works for minimal accepted size of names list.", {
  expect_that(getNamesFromOrdered(FALSE, letters[1:2]), equals(list(x = 'b', y = 'a', w = NULL)))
  expect_that(getNamesFromOrdered(TRUE, letters[1:3]), equals(list(x = 'b', y = 'a', w = 'c')))
})

test_that("getNamesFromOrdered works on larger names list.", {
  expect_that(getNamesFromOrdered(FALSE, letters), equals(list(x = letters[2:26], y = 'a', w = NULL)))
  expect_that(getNamesFromOrdered(TRUE, letters), equals(list(x = letters[2:25], y = 'a', w = 'z')))
})

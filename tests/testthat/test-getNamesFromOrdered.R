context("getNamesFromOrdered testing")

test_that("getNamesFromOrdered throws errors when list of names is too small.", {
  expect_that(getNamesFromOrdered(letters[1:1], FALSE), throws_error())
  expect_that(getNamesFromOrdered(letters[1:2], TRUE), throws_error())
})

test_that("getNamesFromOrdered works for minimal accepted size of names list.", {
  expect_that(getNamesFromOrdered(letters[1:2], FALSE), equals(list(x = 'b', y = 'a', w = NULL)))
  expect_that(getNamesFromOrdered(letters[1:3], TRUE), equals(list(x = 'b', y = 'a', w = 'c')))
})

test_that("getNamesFromOrdered works on larger names list.", {
  expect_that(getNamesFromOrdered(letters, FALSE), equals(list(x = letters[2:26], y = 'a', w = NULL)))
  expect_that(getNamesFromOrdered(letters, TRUE), equals(list(x = letters[2:25], y = 'a', w = 'z')))
})

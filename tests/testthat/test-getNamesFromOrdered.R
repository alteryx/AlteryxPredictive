context("getNamesFromOrdered testing")

test_that("getNamesFromOrdered throws errors when list of names is too small.", {
  expect_error(getNamesFromOrdered(letters[1:1], FALSE))
  expect_error(getNamesFromOrdered(letters[1:2], TRUE))
})

test_that("getNamesFromOrdered works for minimal accepted size of names list.", {
  expect_equal(
    getNamesFromOrdered(letters[1:2], FALSE),
    list(x = 'b', y = 'a', w = NULL)
  )
  expect_equal(
    getNamesFromOrdered(letters[1:3], TRUE),
    list(x = 'b', y = 'a', w = 'c')
  )
})

test_that("getNamesFromOrdered works on larger names list.", {
  expect_equal(
    getNamesFromOrdered(letters, FALSE),
    list(x = letters[2:26], y = 'a', w = NULL)
  )
  expect_equal(
    getNamesFromOrdered(letters, TRUE),
    list(x = letters[2:25], y = 'a', w = 'z')
  )
})

context("createDTParams testing")

names <- list(
  x = c("a", "b", "c"),
  y = c("y"),
  w = c("w")
)

config <- list(
  minsplit = 1,
  minbucket = 1,
  xval = 1,
  maxdepth = 1,
  cp = .01,
  used.weights = TRUE,
  select.type = TRUE,
  classification = TRUE,
  use.gini = TRUE,
  usesurrogate.0 = FALSE,
  usesurrogate.1 = TRUE,
  usesurrogate.2 = FALSE,
  method = "class",
  maxNumBins = 4
)

test_that("non XDF results give expected param list", {
  xdf_properties <- list(
    is_XDF = FALSE,
    xdf_path = ""
  )

  expect_that(createDTParams(config, names, xdf_properties), is_equivalent_to(
    list(
      is_XDF = FALSE,
      xdf_path = "",
      minsplit = 1,
      minbucket = 1,
      xval = 1,
      maxdepth = 1,
      method = "class",
      cp = .01,
      data = quote(the.data),
      formula = formula("y ~ a + b + c"),
      weights = c("w"),
      parms = list(split = "gini"),
      usesurrogate = 1
    )
  ))
})

test_that("XDF results give expected param list", {
  xdf_properties <- list(
    is_XDF = TRUE,
    xdf_path = "xdf"
  )

  expect_that(createDTParams(config, names, xdf_properties), is_equivalent_to(
    list(
      is_XDF = TRUE,
      xdf_path = "xdf",
      minsplit = 1,
      minbucket = 1,
      xval = 1,
      maxdepth = 1,
      method = "class",
      cp = .01,
      data = quote(the.data),
      formula = formula("y ~ a + b + c"),
      weights = c("w"),
      parms = list(split = "gini"),
      usesurrogate = 1,
      maxNumBins = 4
    )
  ))
})

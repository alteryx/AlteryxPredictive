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
  trials = 1,
  rules = FALSE,
  subset = FALSE,
  bands = 0,
  bands.check = FALSE,
  winnow = FALSE,
  CF = .25,
  minCases = 2,
  fuzzyThreshold = FALSE,
  sample = 0,
  seed = 1,
  earlyStopping = TRUE,
  cp = .01,
  used.weights = TRUE,
  select.type = TRUE,
  classification = TRUE,
  use.gini = TRUE,
  usesurrogate.0 = FALSE,
  usesurrogate.1 = TRUE,
  usesurrogate.2 = FALSE,
  method = "class",
  maxNumBins = 4,
  total.correct = TRUE,
  GlobalPruning = TRUE
)

test_that("non XDF results give expected param list", {
  xdf_properties <- list(
    is_XDF = FALSE,
    xdf_path = ""
  )

  expect_that(createDTParams(config, names), is_equivalent_to(
    list(
      minsplit = 1,
      minbucket = 1,
      xval = 1,
      maxdepth = 1,
      trials = 1,
      rules = FALSE,
      subset = FALSE,
      bands = 0,
      winnow = FALSE,
      CF = .25,
      minCases = 2,
      fuzzyThreshold = FALSE,
      sample = 0,
      seed = 1,
      earlyStopping = TRUE,
      cp = .01,
      data = quote(the.data),
      formula = formula("y ~ a + b + c"),
      weights = as.symbol("w"),
      method = "class",
      parms = list(split = "gini"),
      usesurrogate = 1,
      maxNumBins = 4,
      noGlobalPruning = FALSE,
      surrogatestyle = 0
    )
  ))
})

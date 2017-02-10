context("Testing that df2NumericMatrix demonstrates correct behavior when provided a column as input")

x <- mtcars$mpg
test_that("df2NumericMatrix converts a numeric vector to a matrix when convertVectorToDataFrame = TRUE",
          expect_equal(
            (df2NumericMatrix(
              x = mtcars$mpg,
              filtering_message = NULL,
              convertVectorToDataFrame = TRUE
              )
            ),
          as.matrix(as.data.frame(x))
            )
          )

test_that("df2NumericMatrix returns an appropriate error when convertVectorToDataFrame = TRUE",
          expect_error(
            df2NumericMatrix(
              x = mtcars$mpg,
              filtering_message = NULL,
              convertVectorToDataFrame = FALSE
              )
            )
          )

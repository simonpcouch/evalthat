test_that("expect_loads works", {
  x <- "library(tibble)\ntibble(x=1)"
  expect_equal(expect_loads(x, "tibble"), x)
  expect_snapshot(error = TRUE, expect_loads(x, "dplyr"))

  x <- "tibble(x=1)"
  expect_snapshot(error = TRUE, expect_loads(x, "tibble"))
})

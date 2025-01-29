test_that("expect_r_code works", {
  expect_success(expect_r_code("x <- 1"))
  expect_success(expect_r_code("library(tibble)\ntibble(x=1)"))

  expect_failure(expect_r_code("x <-"))
  expect_failure(expect_r_code("x <- )"))
  expect_failure(expect_r_code("x <- )"))
})

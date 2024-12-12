test_that("expect_r_code works", {
  expect_success(expect_r_code("x <- 1"))
  expect_success(expect_r_code("library(tibble)\ntibble(x=1)"))

  expect_failure(expect_r_code("x <-"))
  expect_failure(expect_r_code("x <- )"))
  expect_failure(expect_r_code("x <- )"))
})

test_that("expect_calls works", {
  skip("Currently not planning on exporting.")

  expect_success(expect_calls("a <- mean(1:10)", "mean"))
  expect_success(expect_calls("mean(1:10); mean(2:20)", "mean"))
  expect_success(expect_calls("mean(x = c(1,2,3), trim = 0.1, na.rm = TRUE)", "mean"))
  # expect_success(expect_calls("tibble::new_tibble(list())", "new_tibble"))

  expect_failure(expect_calls("a <- list(1:10)", "mean"))
  expect_failure(expect_calls("a <- \"mean\"", "mean"))
})

test_that("expect_loads works", {
  skip("Currently not planning on exporting.")

  expect_success(expect_loads("library(tibble)\ntibble(x=1)", "tibble"))

  expect_failure(expect_loads("library(tibble)\ntibble(x=1)", "dplyr"))
  expect_failure(expect_loads("tibble(x=1)", "tibble"))
  # TODO: what should it do if the needed fns are just namespaced?
})

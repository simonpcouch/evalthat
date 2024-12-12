test_that("input() stores value and returns it", {
  evalthat_env$input <- NULL
  expect_equal(input("foo"), "foo")
  expect_equal(evalthat_env$input, "foo")
})

test_that("output() stores value and returns it", {
  evalthat_env$output <- NULL
  expect_equal(output("bar"), "bar")
  expect_equal(evalthat_env$output, "bar")
})

test_that("recognizes eval files", {
  expect_true(is_eval_file("tests/evalthat/test-something.R"))
  expect_true(is_eval_file("path/to/evalthat/test-something.r"))
  expect_true(is_eval_file("evalthat/test-foo.R"))
})

test_that("rejects non-eval files", {
  expect_false(is_eval_file("tests/testthat/test-something.R"))
  expect_false(is_eval_file("tests/evalthat/something.R"))
  expect_false(is_eval_file("tests/evalthat/not-test-something.R"))
})

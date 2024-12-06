test_that("recognizes eval files", {
  expect_true(is_eval_file("tests/evalthat/test-something.R"))
  expect_true(is_eval_file("path/to/evalthat/test-something.r"))
  expect_true(is_eval_file("evalthat/test-foo.R"))
  expect_true(is_eval_file("test-something.R"))
})

test_that("rejects non-eval files", {
  expect_false(is_eval_file("something.R"))
  expect_false(is_eval_file("not-test-something.R"))
})

test_that("eval_files fails informatively with nonexistent path", {
  expect_snapshot(eval_files("doesntexist"), error = TRUE)
  expect_snapshot(eval_files("./notthere"), error = TRUE)
})

test_that("eval_files processes file path", {
  withr::with_tempdir({
    writeLines("", "test-something.R")
    expect_equal(
      eval_files("test-something.R"),
      list(eval_dir = ".", eval_files = "test-something.R")
    )
  })
})

test_that("eval_files processes directory path", {
  withr::with_tempdir({
    dir.create("tests")
    writeLines("", "tests/test-something.R")
    writeLines("", "tests/test-other.R")
    writeLines("", "tests/not-a-test.R")

    expect_equal(
      eval_files("tests"),
      list(eval_dir = "tests", eval_files = c("test-other.R", "test-something.R"))
    )
  })
})

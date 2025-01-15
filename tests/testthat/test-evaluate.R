test_that("`evaluate()` returns results as a tibble (eval reporter)", {
  capture_output_lines(
    results <- evaluate(test_path("reporters", "tests.R"), repeats = 2)
  )

  expect_s3_class(results, "tbl_df")
  expect_equal(nrow(results), 2)
  expect_named(
    results,
    c("something", "pct", "n_fail", "n_pass", "timestamp", "file_hash",
      "io", "problems"),
    ignore.order = TRUE
  )
})

test_that("`evaluate()` returns results as a tibble (compact reporter)", {
  capture_output_lines(
    results <- evaluate(test_path("reporters", "tests.R"), repeats = 1)
  )

  expect_s3_class(results, "tbl_df")
  expect_equal(nrow(results), 1)
  expect_named(
    results,
    c("something", "pct", "n_fail", "n_pass", "timestamp", "file_hash",
      "io", "problems"),
    ignore.order = TRUE
  )
})

test_that("`evaluate_active_file()` returns results as a tibble (eval reporter)", {
  capture_output_lines(
    results <- evaluate_active_file(test_path("reporters", "tests.R"), repeats = 2)
  )

  expect_s3_class(results, "tbl_df")
  expect_equal(nrow(results), 2)
  expect_named(
    results,
    c("something", "pct", "n_fail", "n_pass", "timestamp", "file_hash",
      "io", "problems"),
    ignore.order = TRUE
  )
})

test_that("`evaluate_active_file()` returns results as a tibble (compact reporter)", {
  capture_output_lines(
    results <- evaluate_active_file(test_path("reporters", "tests.R"), repeats = 1)
  )

  expect_s3_class(results, "tbl_df")
  expect_equal(nrow(results), 1)
  expect_named(
    results,
    c("something", "pct", "n_fail", "n_pass", "timestamp", "file_hash",
      "io", "problems"),
    ignore.order = TRUE
  )
})

test_that("results tibbles are returned invisibly", {
  capture_output_lines(
    expect_invisible(
      evaluate(test_path("reporters", "tests.R"), repeats = 1)
    )
  )

  capture_output_lines(
    expect_invisible(
      evaluate(test_path("reporters", "tests.R"), repeats = 2)
    )
  )

  capture_output_lines(
    expect_invisible(
      evaluate_active_file(test_path("reporters", "tests.R"), repeats = 1)
    )
  )
})

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

test_that("`evaluate()` errors informatively with non-function eval", {
  expect_snapshot(
    error = TRUE,
    results <- evaluate(test_path("reporters", "no-function-returned.R"), repeats = 2)
  )
})

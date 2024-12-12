test_that("EvalProgressReporter works", {
  local_output_override()

  results_path <- test_path("reporters", "_results")
  withr::defer(unlink(results_path, recursive = TRUE))
  expect_snapshot_reporter(
    EvalProgressReporter$new(update_interval = 0, min_time = Inf),
    test_path("reporters/tests.R"),
    repeats = 2
  )

  expect_true(dir.exists(results_path))
})

test_that("EvalCompactProgressReporter works", {
  local_output_override()

  results_path <- test_path("reporters", "_results")
  withr::defer(unlink(results_path, recursive = TRUE))
  expect_snapshot_reporter(
    EvalCompactProgressReporter$new(),
    test_path("reporters/tests.R"),
    repeats = 1
  )

  expect_true(dir.exists(results_path))
})

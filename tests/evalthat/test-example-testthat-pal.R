skip_if_not_installed("pal")
skip_if_offline()
skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))

testthat_pal <- pal::.init_pal("testthat")

evaluating(
  testthat_pal$.__enclos_env__$private$Chat$.__enclos_env__$private$provider@model,
  "transitioning to testthat 3e"
)

test_that("testthat pal works for transitioning to snapshots", {
  testthat_pal <- pal::.init_pal("testthat")
  input <- input("
  expect_warning(
    check_ellipses(\"exponentiate\", \"tidy\", \"boop\", exponentiate = TRUE, quick = FALSE),
    \"\\`exponentiate\\` argument is not supported in the \\`tidy\\(\\)\\` method for \\`boop\\` objects\"
  )
  ")
  output <- output(testthat_pal$chat(input))

  expect_r_code(output)
  expect_match(output, "expect_snapshot")
  expect_match(output, "check_ellipses")
  # assign result to intermediate output so as not to also newly test
  # output other than the raised condition
  expect_match(output, " <- ")

  # a warning rather than an error
  expect_no_match(output, "error = TRUE")
  expect_no_match(output, "expect_warning")
})

test_that("testthat pal works for transitioning to multiple snapshots", {
  testthat_pal <- pal::.init_pal("testthat")
  input <- input("
    augment_error <- \"augment is only supported for fixest models estimated with feols, feglm, or femlm\"
    expect_error(augment(res_fenegbin, df), augment_error)
    expect_error(augment(res_feNmlm, df), augment_error)
    expect_error(augment(res_fepois, df), augment_error)
  ")
  output <- output(testthat_pal$chat(input))

  expect_r_code(output)
  expect_match(output, "expect_snapshot")
  expect_match(output, "error = TRUE")
  expect_match(output, "augment(", fixed = TRUE)

  # no need to assign result to intermediate output as the cnd is an error
  expect_no_match(output, "<- ")

  expect_no_match(output, "expect_error")
  expect_no_match(output, "expect_snapshot_error")
})

test_that("testthat pal can handle regexp = NA", {
  testthat_pal <- pal::.init_pal("testthat")
  input <- input("
    expect_error(
      p4_b <- check_parameters(w4, p4_a, data = mtcars),
      regex = NA
    )
  ")
  output <- output(testthat_pal$chat(input))

  expect_r_code(output)
  expect_match(output, "expect_no_error")
  expect_match(output, "p4_b <- check_parameters")

  # there is no error to snapshot, but expect_error might lead one to
  # believe there is
  expect_no_match(output, "expect_snapshot")
  expect_no_match(output, "error = TRUE")
})

test_that("testthat pal doesn't touch tests for error classes", {
  testthat_pal <- pal::.init_pal("testthat")
  input <- input("
    expect_error(tidy(pca, matrix = \"u\"), class = \"pca_error\")
  ")
  output <- output(testthat_pal$chat(input))

  expect_r_code(output)
  expect_match(output, "expect_error(tidy", fixed = TRUE)
  expect_match(output, "class =")

  # do not want to make any changes--tests for specific error
  # classes are well-defined
  expect_no_match(output, "expect_snapshot")
  expect_no_match(output, "error = TRUE")
})

test_that("testthat pal can disentangle nested expectations", {
  testthat_pal <- pal::.init_pal("testthat")
  # in this example, we have a warning to snapshot test and two results
  # to comparse
  input <- input("
    expect_equal(
      fit_resamples(decision_tree(cost_complexity = 1), bootstraps(mtcars)),
      expect_warning(tune_grid(decision_tree(cost_complexity = 1), bootstraps(mtcars)))
    )
  ")
  output <- output(testthat_pal$chat(input))

  expect_r_code(output)
  expect_match(output, "expect_snapshot")
  expect_match(output, "expect_equal")

  # in order to disentangle this expectation, at least one assignment is necessary
  expect_match(output, " <- ")
})

test_that("testthat pal transitions from snapshot tests that don't test context", {
  testthat_pal <- pal::.init_pal("testthat")
  # in this example, we have a warning to snapshot test and two results
  # to comparse
  input <- input("
    expect_snapshot_error(
      fit_best(knn_pca_res, parameters = tibble(neighbors = 2))
    )
  ")
  output <- output(testthat_pal$chat(input))

  expect_r_code(output)
  expect_match(output, "expect_snapshot")
  expect_match(output, "error = TRUE")

  expect_no_match(output, "expect_snapshot_error")

  # leaves input as-is
  expect_match(output, "fit_best(knn_pca_res", fixed = TRUE)
})

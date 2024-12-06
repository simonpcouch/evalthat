#' Evaluate LLM performance across models/prompts/etc
#'
#' `evaluate_across()` is an extension of [evaluate()] and friends that
#' allows users to run evals across various combinations of models, prompts,
#' or any other arbitrary parameter.
#'
#' @inheritParams evaluate
#' @param across A data frame where each column represents an option to be set
#' when evaluating the file at `path` and each row represents a pass through
#' that file.
#'
#' @examplesIf FALSE
#' # evaluate a directory of evals across several models
#' evaluate_across(
#'   tibble::tribble(
#'     ~provider,          ~model,
#'     elmer::chat_openai, "gpt-4o",
#'     elmer::chat_openai, "gpt-4o-mini",
#'     elmer::chat_claude, "claude-3-5-sonnet-latest",
#'     elmer::chat_ollama, "qwen2.5-coder:14b"
#'   )
#' )
#'
#' # in the eval file, write...
#' ```r
#' provider <- getOption("provider", default = elmer::chat_claude)
#' model <- getOption("model", default = "claude-3-5-sonnet-latest")
#'
#' chat <- provider(model = model)
#'
#' # ... and so on
#' ```
#' @export
evaluate_across <- function(path = ".", across = tibble(), repeats = 1L, ...) {
  check_data_frame(across)

  reporter <- EvalProgressReporter$new()

  reporter$start_reporter()
  withr::defer(reporter$end_reporter())
  for (i in seq_len(nrow(across))) {
    withr::with_options(
      across[i,],
      evaluate_file_impl(path = path, repeats = repeats, reporter = reporter, ...)
    )
  }
}

# copied from testthat so that we can set `start_end_reporter = FALSE` in `with_reporter()`
test_files_serial <- function(test_dir,
                              test_package,
                              test_paths,
                              load_package,
                              reporter,
                              env = NULL,
                              desc = NULL,
                              stop_on_failure = FALSE,
                              stop_on_warning = FALSE,
                              error_call = caller_env()) {
  env <- testthat:::test_files_setup_env(
    test_package,
    test_dir,
    load_package,
    env
  )
  testthat:::local_testing_env(env)

  reporters <- testthat:::test_files_reporter(reporter)
  testthat:::with_reporter(
    reporters$multi,
    lapply(
      test_paths,
      testthat:::test_one_file,
      env = env,
      desc = desc,
      error_call = error_call
    ),
    start_end_reporter = FALSE
  )
  testthat:::test_files_check(
    reporters$list$get_results(),
    stop_on_failure = stop_on_failure,
    stop_on_warning = stop_on_warning
  )
}


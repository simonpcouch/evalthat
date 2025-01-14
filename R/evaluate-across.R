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
#' # evaluate a directory of evals across several models,
#' # repeating each eval twice
#' eval <- evaluate_across(
#'   "tests/evalthat/test-ggplot2.R",
#'   across = tibble(chat = c(
#'     chat_openai(model = "gpt-4o-mini", echo = FALSE),
#'     chat_claude(model = "claude-3-5-sonnet-latest", echo = FALSE))
#'   ),
#'   repeats = 2
#' )
#' @export
evaluate_across <- function(path = ".", across = tibble(), repeats = 1L, ...) {
  check_data_frame(across)

  reporter <- EvalProgressReporter$new()

  eval_files <- eval_files(path = path)

  reporter$start_reporter()
  withr::defer(reporter$end_reporter())
  purrr::pmap(
    across,
    ~test_files_serial(
      test_dir = eval_files$eval_dir,
      test_package = NULL,
      load_package = "none",
      test_paths = rep(
        file.path(eval_files$eval_dir, eval_files$eval_files),
        times = repeats
      ),
      env = new_environment(data = list(...), test_env()),
      reporter = reporter
    )
  )

  invisible(results_tibble())
}

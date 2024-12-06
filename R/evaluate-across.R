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
#' evaluate_across(
#'   tibble(chat = c(
#'     chat_openai(model = "gpt-4o-mini", echo = FALSE),
#'     chat_claude(model = "claude-3-5-sonnet-latest", echo = FALSE))
#'   ),
#'   repeats = 2
#' )
#'
#' # in the eval file, write...
#' chat <- getOption(
#'   "chat",
#'   default = list(elmer::chat_claude("claude-3-5-sonnet-latest", echo = FALSE))
#' )[[1]]
#'
#' evaluating(model = str(chat))
#'
#' # ... and so on
#' @export
evaluate_across <- function(path = ".", across = tibble(), repeats = 1L, ...) {
  check_data_frame(across)

  reporter <- EvalProgressReporter$new()

  eval_files <- eval_files(path = path)

  reporter$start_reporter()
  withr::defer(reporter$end_reporter())
  for (i in seq_len(nrow(across))) {
    withr::with_options(
      across[i,],
      evaluate_impl(path = path, repeats = repeats, reporter = reporter, ...)
    )
  }
}

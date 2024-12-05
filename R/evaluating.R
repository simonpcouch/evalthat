#' Set context for evaluation
#'
#' This helper is analogous to [testthat::context()] and sets metadata
#' for the test for logging. Arguments passed to this function, including via
#' `...`, will logged in the eval's _result file_, readable with `results_read()`.
#'
#' TODO: this will be most helpful when `evaluate_across()` is available. at
#' that point, explain how to iterate across models/prompts etc by programatically
#' passing values to this function.
#'
# TODO: support either of these being null?
# TODO: possibly infer task from the file-name, as `context()` now does?
#' @param model A single string describing the model used to generate output.
#' @param task A single string describing the task being evaluated on.
#' @param ... Additional named fields describing eval metadata, e.g. the prompt
#' version, model provider, etc.
#'
#' @export
evaluating <- function(model, task, ...) {
  check_string(task)
  check_string(model)

  context <- c(list(model = model, task = task), list(...))

  reporter <- get_reporter()
  if (!is.null(reporter)) {
    get_reporter()$.start_context(context)
  }
  invisible(format_context(context))
}

format_context <- function(context) {
  desc <- cli::format_inline(
    "Evaluating {.field {context$model}} for {cli::col_blue(context$task)}."
  )
  desc
}

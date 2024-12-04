
#' Set context for evaluation task
#'
#' This helper is analogous to `testthat::context()` and sets metadata
#' for the test for logging.
#'
#' @param model A single string describing the model used to generate output.
#' @param task A single string describing the task being evaluated on.
#' @param ... TODO: document
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

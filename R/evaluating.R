
#' Set context for evaluation task
#'
#' This helper is analogous to `testthat::context()` and sets metadata
#' for the test for logging.
#'
#' @param model A single string describing the model used to generate output.
#' @param task A single string describing the task being evaluated on.
#'
#' @returns
#' A description of the format "Evaluating {model} for {task}",
#' returned invisibly.
#'
#' @export
evaluating <- function(model, task) {
  check_string(task)
  check_string(model)
  desc <- cli::format_inline(
    "Evaluating {.field {model}} for {cli::col_blue(task)}."
  )

  reporter <- get_reporter()
  if (!is.null(reporter)) {
    get_reporter()$.start_context(desc)
  }
  invisible(desc)
}

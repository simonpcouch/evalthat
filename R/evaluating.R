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
#' @param ... Named fields describing eval metadata, e.g. the model provider,
#' model, prompt version, model provider, etc.
#'
#' @export
evaluating <- function(...) {
  context <- list(...)

  reporter <- get_reporter()
  if (!is.null(reporter)) {
    get_reporter()$.start_context(context)
  }

  invisible(context)
}

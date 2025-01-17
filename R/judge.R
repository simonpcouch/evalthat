#' Define judge models
#'
#' @description
#' Judge models allow for automated grading of model output
#' (with `grade_model()`) by passing responses to other LLM judges.
#'
#' To learn more about how evalthat makes use of judge models, see
#' `vignette("Model grading", package = "evalthat")`.
#'
#' @param ... Named (optionally) [ellmer::Chat()] objects, e.g. the output of
#' [ellmer::chat_openai()] or [ellmer::chat_claude()].
#'
#' @examples
#' library(ellmer)
#'
#' j <- judges(
#'   gpt4o = chat_openai(model = "gpt-4o"),
#'   claude = chat_claude()
#' )
#'
#' j
#'
#' # doesn't necessarily need to be named:
#' judges(chat_openai(model = "gpt-4o"), chat_claude())
#' @export
judges <- function(...) {
  res <- dots_list(..., .named = TRUE)
  for (i in seq_along(judges)) {
    check_judge(res[[i]], names(res)[i])
  }

  structure(res, class = "judges")
}

check_judge <- function(x, x_name, call = caller_env()) {
  if (!inherits(x, "Chat")) {
    cli::cli_abort(
      "{.arg {x_name}} must be an ellmer Chat, not a {.obj_type_friendly {x}}",
      call = call
    )
  }

  invisible()
}

#' @export
print.judges <- function(x, ...) {
  cli::cat_line(cli::format_inline("Model judge{?s} {cli::col_blue(names(x))}."))
}


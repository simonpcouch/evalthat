
#' Flag model inputs and outputs
#'
#' When evaluating model input in evaluations, flag the input with `input()`
#' and the output with `output()`, allowing for persistent logging of
#' responses.
#'
#' @param x A character string representing either the model input or output.
#'
#' @returns `x`, as provided. Called for its side effect.
#'
#' @examplesIf FALSE
#' test_that("the model works", {
#'   input <- input("some input")
#'   output <- output(some_function_call_generating_response(input))
#' })
#'
#' @name io
#' @export
input <- function(x) {
  evalthat_env$input <- x

  x
}

#' @rdname io
output <- function(x) {
  evalthat_env$output <- x

  x
}

evalthat_env <- new_environment()

error_no_reporter <- function(fn, call = caller_env()) {
  cli::cli_abort(
    "{.fn {fn}} must be called inside of a testing context.",
    call = call
  )
}

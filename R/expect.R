#' Check if input is syntactically valid R code
#'
#' @param object A single string, possibly containing parse-able R code.
#'
#' @details
#' This helper does not attempt to evaluate the code in the provided string.
#' Instead, it uses [parse()] to determine whether the string
#' contains valid R syntax.
#'
#' This expectation will fail if `object` contains any exposition surrounding
#' the code, including if code is surrounded by backticks.
#'
#' @returns
#' Returns nothing but throws an error if the input is not valid R code.
#'
#' @export
expect_r_code <- function(object) {
  parsed <- tryCatch(parse(text = object), error = function(e) e)

  expect(
    !inherits(parsed, "error"),
    "Output was not syntactically valid R code."
  )
}

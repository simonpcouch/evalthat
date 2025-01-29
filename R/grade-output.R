#' Grade model outputs
#'
#' @param target A single string describing the desired output. Set to `NULL`
#' to omit desired output, though this is discouraged unless you are
#' grading the output yourself.
#'
#' @returns
#'
#' @export
grade_output <- function(target) {
  check_string(target)

  reporter <- testthat::get_reporter()
  o_apply(reporter$reporters, "cache_for_grading", target)

  invisible()
}

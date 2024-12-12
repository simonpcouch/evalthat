# copied from testthat
local_output_override <- function(width = 80,
                                  crayon = TRUE,
                                  unicode = TRUE,
                                  .env = parent.frame()) {
  reporter <- get_reporter()
  if (is.null(reporter)) {
    return()
  }

  old_width <- reporter$width
  old_crayon <- reporter$crayon
  old_unicode <- reporter$unicode

  reporter$width <- width
  reporter$crayon <- crayon
  reporter$unicode <- unicode

  withr::defer({
    reporter$width <- old_width
    reporter$crayon <- old_crayon
    reporter$unicode <- old_unicode
  }, .env)
}

local_rng_version <- function(version, .local_envir = parent.frame()) {
  withr::defer(RNGversion(as.character(getRversion())), envir = .local_envir)
  suppressWarnings(RNGversion(version))
}

expect_snapshot_reporter <- function(reporter,
                                     paths = test_path("reporters/tests.R"),
                                     ...) {
  local_options(rlang_trace_format_srcrefs = FALSE)
  local_rng_version("3.3")
  set.seed(1014)

  testthat::expect_snapshot_output(
    for (path in paths) evaluate_impl(path, reporter = reporter, ...)
  )
}

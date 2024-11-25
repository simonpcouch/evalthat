expect_syntactically_valid <- function(object) {
  # TODO: treesitter.r-fu
  expect(TRUE, "Output was not syntactically valid.")
}

expect_calls <- function(object, fn) {
  # TODO: treesitter.r-fu
  invisible(object)
}

expect_n_calls <- function(object, n) {
  # TODO: treesitter.r-fu
  invisible(object)
}

expect_loads <- function(object, pkg) {
  act <- quasi_label(rlang::enquo(object), arg = "object")

  # TODO: actually use treesitter.r-fu
  act$loads <- grepl(paste0("library(", pkg, ")"), object, fixed = TRUE)
  expect(
    act$loads,
    failure_message = cli::format_inline("Code does not load the {.pkg {pkg}} package.")
  )

  invisible(act$val)
}

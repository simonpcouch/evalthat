#' Check if input is syntactically valid R code
#'
#' @param object A single string, possibly containing parse-able R code.
#'
#' @details
#' This helper does not attempt to evaluate the code in the provided string.
#' Instead, it uses [treesitter::parser_parse()] to determine whether the string
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
  tree <- treesitter::parser_parse(r_parser, object)
  node <- treesitter::tree_root_node(tree)

  # TODO: i think this (incorrectly?) fails for multiple lines of code
  expect(
    !treesitter::node_has_error(node),
    "Output was not syntactically valid R code."
  )
}

# nocov start
# TODO: should this be able to detect having been called programmatically?
# e.g. lapply(list(a = 1:2, b = 3:4), mean)
expect_calls <- function(object, fn) {
  query <- treesitter::query(r_language, "(call function: (_) @func_name)")
  tree <- treesitter::parser_parse(r_parser, object)
  node <- treesitter::tree_root_node(tree)
  captures <- treesitter::query_captures(query, node)

  expect(
    fn %in% sapply(captures$node, treesitter::node_text),
    "Function {.fn} was not called."
  )

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
# nocov end

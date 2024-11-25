expect_r_code <- function(object) {
  # object <- "cli::cli_abort(\"hey there\")"

  tree <- treesitter::parser_parse(r_parser, object)
  node <- treesitter::tree_root_node(tree)

  expect(
    !treesitter::node_has_error(node),
    "Output was not syntactically valid R code."
  )
}

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

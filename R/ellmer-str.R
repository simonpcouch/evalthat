#' @export
str.Chat <- function(object, ...) {
  provider <- object$.__enclos_env__$private$provider

  # ollama uses the OpenAI provider format
  if (identical(provider@api_key, "ollama")) {
    provider_name <- "ollama"
  } else {
    provider_name <- sub("ellmer::Provider", "", class(provider)[1])
  }

  model <- provider@model

  extra_args <- provider@extra_args
  if (length(extra_args) == 0) {
    extra_args <- ""
  } else {
    extra_args <- purrr::map2_chr(names(extra_args), extra_args, paste, sep = ": ")
    extra_args <- paste0(" (", paste(extra_args, collapse = ", "), ")")
  }

  cli::format_inline(
    "{provider_name} {model}{extra_args}"
  )
}

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' @export
autoplot.evals_df <- function(object, ..., type = c("within", "across")) {
  type <- rlang::arg_match(type)
  if (identical(type, "across")) {
    cli::cli_abort("{.code type = 'across'} not implemented yet.")
  }

  eval_bare(call2(paste0("autoplot_", type), object = object, ...))
}

autoplot_within <- function(object, ...) {
  model <- object$model[1]
  task <- object$task[1]
  file_hash <- object$file_hash[1]

  if (dplyr::n_distinct(
    object[c("model", "task", "evaluating_extras", "file_hash")]) > 1
    ) {
    object <- dplyr::arrange(object, desc(timestamp))
    model <- object$model[1]
    task <- object$task[1]
    file_hash <- object$file_hash[1]
    cli::cli_warn(
      "Displaying results for most recent eval file on model
       {.field {model}} for {cli::col_blue(task)}.",
      i = "Filter results down to one result set manually to silence this warning."
    )
    object <- vctrs::vec_slice(
      object,
      object$model == model & object$task == task & object$file_hash == file_hash
    )
  }

  ggplot2::ggplot(object) +
    ggplot2::aes(x = pct) +
    ggplot2::geom_histogram() +
    ggplot2::labs(
      subtitle = cli::format_inline("Eval of {model} for {task}")
    ) +
    ggplot2::xlim(0, 1)
}

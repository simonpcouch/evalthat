#' Interface with eval results
#'
#' @param dir Optional. A single string specifying a subdirectory of
#' `tests/evalthat/_results`. If `NULL`, will read each subdirectory and
#' row-bind the results.
#'
#' @returns
#' A tibble containing the combined results from result files in the
#' specified directory. Evaluations are ordered chronologically, with most
#' recent results on top.
#'
#' @export
results_read <- function(dir = NULL) {
  dir <- paste0("tests/evalthat/_results", dir, collapse = "/")
  result_files <- list.files(dir, full.names = TRUE, recursive = TRUE)

  res <- tibble::tibble()
  for (file in result_files) {
    res <- vctrs::vec_rbind(res, qs::qread(file))
  }

  dplyr::arrange(res, desc(timestamp)) %>%
    tidyr::unnest_wider(col = evaluating) %>%
    structure(class = c("evals_df", class(res)))
}

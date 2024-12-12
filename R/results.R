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

  res <- list()
  for (file in result_files) {
    res <- c(res, qs::qread(file))
  }

  results_tibble(res)
}

results_tibble <- function(results = evalthat_env$last_result) {
  res <- tibble::tibble()
  for (result in results) {
    res <- vctrs::vec_rbind(res, result)
  }

  if (nrow(res) == 0) {return(evals_df(res))}

  res <- dplyr::arrange(res, desc(timestamp))
  res <- tidyr::unnest_wider(res, col = evaluating)
  evals_df(res)
}

evals_df <- function(tbl) {
  structure(tbl, class = c("evals_df", class(tbl)))
}

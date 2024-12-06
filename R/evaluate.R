#' Evaluate LLM performance
#'
#' @description
#' `evaluate()` and `evaluate_active_file()` are roughly analogous to
#' [devtools::test()] and [devtools::test_active_file()], though note that
#' `evaluate()` can take either a directory of files or a single file.
#'
#' @param repeats A single positive integer specifying the number of
#' evaluation repeats, or runs over the same test files. Assuming that the
#' models you're evaluating provide non-deterministic output, running the
#' same test files multiple times by setting `repeats > 1` will help you
#' quantify the variability of your evaluations.
#' @param path Path to the directory or file in question. Optional.
#' @param ... Additional arguments passed to internal functions.
#'
#' @returns
#' Results of the evaluation, invisibly. Mostly called for its side-effects:
#'
#' * An interactive progress interface tracking results in real-time.
#' * _Result files_ are stored in `dirname(path)/_results`. Result files contain
#' persistent, fine-grained evaluation results and can be interfaced with
#' via [results_read()] and friends.
#'
#' @export
evaluate <- function(path = ".", repeats = 1L, ...) {
  evaluate_impl(path = path, repeats = repeats, ...)
}

#' @rdname evaluate
#' @export
evaluate_active_file <- function(path = active_eval_file(), repeats = 1L, ...) {
  evaluate_impl(path = path, repeats = repeats, ...)
}

evaluate_impl <- function(path,
                          repeats,
                          ...,
                          reporter = NULL,
                          call = caller_env()) {
  check_number_whole(repeats, min = 1, allow_infinite = FALSE, call = call)

  eval_files <- eval_files(path)

  withr::local_envvar(devtools::r_env_vars())

  if (repeats == 1 &&
      is.null(reporter) &&
      length(eval_files$eval_files) == 1) {
    reporter <- EvalCompactProgressReporter$new()
  } else {
    reporter <- reporter %||% EvalProgressReporter$new()
  }

  reporter$start_reporter()
  withr::defer(reporter$end_reporter())
  test_files_serial(
    test_dir = eval_files$eval_dir,
    test_package = NULL,
    load_package = "none",
    test_paths = rep(
      file.path(eval_files$eval_dir, eval_files$eval_files),
      times = repeats
    ),
    reporter = reporter
  )
}

active_eval_file <- function(arg = "file", call = parent.frame()) {
  if (!is_rstudio_running()) {
    cli::cli_abort(
      "Argument {.arg {arg}} is missing, with no default",
      call = call
    )
  }
  test_file <- normalizePath(rstudioapi::getSourceEditorContext()$path)

  if (!is_eval_file(test_file)) {
    cli::cli_abort(
      "The active file must an R file named beginning with {.field test-}.",
      call = call
    )
  }

  test_file
}

is_eval_file <- function(path) {
  grepl("^test-", basename(path)) & grepl("\\.[Rr]$", basename(path))
}

is_rstudio_running <- function() {
  !is_testing() && rstudioapi::isAvailable()
}

eval_files <- function(path, call = caller_env()) {
  if (!dir.exists(path) && !file.exists(path)) {
    cli::cli_abort("{.arg path} does not exist.")
  }

  if (!dir.exists(path)) {
    return(list(eval_dir = dirname(path), eval_files = basename(path)))
  }

  eval_files <- list.files(path)
  list(eval_dir = path, eval_files = eval_files[is_eval_file(eval_files)])
}

# copied from testthat so that we can set `start_end_reporter = FALSE` in `with_reporter()`
test_files_serial <- function(test_dir,
                              test_package,
                              test_paths,
                              load_package,
                              reporter,
                              env = NULL,
                              desc = NULL,
                              stop_on_failure = FALSE,
                              stop_on_warning = FALSE,
                              error_call = caller_env()) {
  env <- testthat:::test_files_setup_env(
    test_package,
    test_dir,
    load_package,
    env
  )
  testthat:::local_testing_env(env)

  reporters <- testthat:::test_files_reporter(reporter)
  testthat:::with_reporter(
    reporters$multi,
    lapply(
      test_paths,
      testthat:::test_one_file,
      env = env,
      desc = desc,
      error_call = error_call
    ),
    start_end_reporter = FALSE
  )
}

#' Evaluate LLM performance
#'
#' @description
#' `evaluate()` and `evaluate_active_file()` are roughly analogous to
#' [devtools::test()] and [devtools::test_active_file()], respectively.
#' Interface with them in the same way that you would with their devtools
#' friends, though note the `epochs` argument—assuming that the models you're
#' evaluating provide non-deterministic output, running the same test files
#' multiple times by setting `epochs > 1` will help you quantify the
#' variability of your evaluations.
#'
#' @param epochs A single positive integer specifying the number of
#' evaluation epochs, or repeats over the same test files.
#' @param pkg,file Path to the package or file in question. Optional.
#  TODO: actually implement this
#' @param filter A string or pattern to filter test files. Optional.
#' @param ... Additional arguments passed to `testthat:::test_files()`.
#'
#' @returns
#' Results of the evaluation, invisibly. Mostly called for its side-effects:
#'
#' * An interactive progress interface tracking results in real-time.
#' * _Result files_ are stored in `evalthat/_results`. Result files contain
#' persistent, fine-grained evaluation results and can be interfaced with
#' via [results_read()] and friends.
#'
#' @export
evaluate <- function(pkg = ".",
                     epochs = 1L,
                     filter = NULL,
                     ...) {
  # devtools:::save_all()
  pkg <- devtools::as.package(pkg)
  check_number_whole(epochs, min = 1, allow_infinite = FALSE)

    # TODO: implement uses_evaltest and use_evalthat

  cli::cli_inform(c(i = "Evaluating {.pkg {pkg$package}}"))
  withr::local_envvar(devtools::r_env_vars())
  load_package <- load_package_for_testing(pkg)

  eval_local(
    pkg$path,
    filter = filter,
    load_package = load_package,
    reporter = EvalProgressReporter$new(),
    epochs = epochs,
    ...
  )
}

#' @rdname evaluate
#' @export
evaluate_active_file <- function(file = active_eval_file(), epochs = 1L, ...) {
  check_number_whole(epochs, min = 1, allow_infinite = FALSE)
  pkg <- devtools::as.package(dirname(file))
  withr::local_envvar(devtools::r_env_vars())
  if (is_rstudio_running()) {
    rstudioapi::executeCommand("activateConsole", quiet = TRUE)
  }
  load_package <- load_package_for_testing(pkg)
  if (identical(epochs, 1L)) {
    testthat::test_file(
      file,
      package = pkg$package,
      load_package = load_package,
      reporter = EvalCompactProgressReporter$new(),
      ...
    )
  } else {
    # todo: does this actually need to be a different reporter? or could
    # `testthat:::test_files()` be exported?
    testthat:::test_files(
      test_dir = "tests/evalthat",
      test_package = pkg$package,
      load_package = load_package,
      test_paths = rep(file, times = epochs),
      reporter = EvalProgressReporter$new()
    )
  }
}

# copied from testthat
load_package_for_testing <- function(pkg) {
  if (pkg$package == "testthat") {
    load_all(pkg$path, quiet = TRUE, helpers = FALSE)
    "none"
  }
  else {
    "source"
  }
}

# # analogous to testthat::test_local
eval_local <- function(path = ".", reporter = NULL, ..., load_package = "source", epochs = 1L) {
  package <- pkgload::pkg_name(path)
  test_path <- file.path(pkgload::pkg_path(path), "tests", "evalthat")
  withr::local_envvar(NOT_CRAN = "true")

  testthat:::test_files(
    test_dir = test_path,
    test_package = NULL,
    test_paths = rep(testthat::find_test_scripts(test_path), each = epochs),
    reporter = EvalProgressReporter$new(),
    ...
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
      "The active file must begin with {.field test-} and live in
       the  {.file tests/evalthat/} directory.",
      call = call
    )
  }

  test_file
}

is_eval_file <- function(path) {
  dir_name <- dirname(path)

  identical(basename(dir_name), "evalthat") &&
  grepl("^test-", basename(path))
}

is_rstudio_running <- function() {
  !is_testing() && rstudioapi::isAvailable()
}

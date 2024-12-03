# analogous to devtools::test
evaluate <- function(pkg = ".",
                     filter = NULL,
                     stop_on_failure = FALSE,
                     export_all = TRUE,
                     ...) {
  # devtools:::save_all()
  pkg <- devtools::as.package(pkg)
  # TODO: implement uses_evaltest
  # if (!uses_testthat(pkg)) {
  #   cli::cli_inform(c(i = "No testing infrastructure found."))
  #   if (!interactive()) {
  #     ui_todo("Setup testing with {ui_code(\"usethis::use_testthat()\")}.")
  #     return(invisible())
  #   }
  #   if (yesno("Create it?")) {
  #     return(invisible())
  #   }
  #   usethis_use_testthat(pkg)
  #   return(invisible())
  # }
  cli::cli_inform(c(i = "Evaluating {.pkg {pkg$package}}"))
  withr::local_envvar(devtools::r_env_vars())
  load_package <- load_package_for_testing(pkg)
  # TODO: could switch to this if we want the tests to actually live in `tests/`
  # but be prefixed with `eval-`
  # testthat::local_mocked_bindings(
  #   find_test_scripts = function(path,
  #                                filter = NULL,
  #                                invert = FALSE,
  #                                ...,
  #                                full.names = TRUE,
  #                                start_first = NULL) {
  #     files <- dir(path, "^eval.*\\.[rR]$", full.names = full.names)
  #     files <- testthat:::filter_test_scripts(files, filter, invert, ...)
  #     testthat:::order_test_scripts(files, start_first)
  #   },
  #   .package = "testthat"
  # )
  eval_local(
    pkg$path,
    filter = filter,
    stop_on_failure = stop_on_failure,
    load_package = load_package,
    reporter = EvalProgressReporter$new(),
    ...
  )
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
eval_local <- function(path = ".", reporter = NULL, ..., load_package = "source") {
  package <- pkgload::pkg_name(path)
  test_path <- file.path(pkgload::pkg_path(path), "tests", "evalthat")
  withr::local_envvar(NOT_CRAN = "true")

  testthat::test_dir(
    test_path,
    package = package,
    reporter = reporter,
    ...,
    load_package = load_package
  )
}

# active file ---------
evaluate_active_file <- function(file = active_eval_file(), ...) {
  pkg <- devtools::as.package(dirname(file))
  withr::local_envvar(devtools::r_env_vars())
  if (is_rstudio_running()) {
    rstudioapi::executeCommand("activateConsole", quiet = TRUE)
  }
  load_package <- load_package_for_testing(pkg)
  testthat::test_file(
    file,
    package = pkg$package,
    load_package = load_package,
    reporter = EvalCompactProgressReporter$new(),
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
       the  {.file tests/evalthat/} directory."
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

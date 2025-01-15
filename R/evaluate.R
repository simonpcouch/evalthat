#' Evaluate LLM performance
#'
#' @description
#' `evaluate()` and `evaluate_active_file()` are roughly analogous to
#' [devtools::test()] and [devtools::test_active_file()], though note that
#' `evaluate()` can take either a directory of files or a single file.
#'
#' @param path Path to the directory or file containing the evaluation code.
#' @param across A data frame where each column represents an option to be set
#' when evaluating the file at `path` and each row represents a pass through
#' that file.
#' @param repeats A single positive integer specifying the number of
#' evaluation repeats, or runs over the same test files. Assuming that the
#' models you're evaluating provide non-deterministic output, running the
#' same test files multiple times by setting `repeats > 1` will help you
#' quantify the variability of your evaluations.
#' @param ... Additional arguments passed to internal functions.
#'
#' @returns
#' Results of the evaluation, invisibly. Evaluation results contain information
#' on the eval metadata as well as numbers of failures
#' and passes, input and output, and descriptions of each failure.
#'
#' The function also has side-effects:
#'
#' * An interactive progress interface tracking results in real-time.
#' * _Result files_ are stored in `dirname(path)/_results`. Result files contain
#' persistent, fine-grained evaluation results and can be interfaced with
#' via [results_read()] and friends.
#'
#' @examplesIf FALSE
#' # evaluate with the default model twice
#' evaluate("tests/evalthat/test-ggplot2.R", repeats = 2)
#'
#' # evaluate a directory of evals across several models,
#' # repeating each eval twice
#' eval <- evaluate(
#'   "tests/evalthat/test-ggplot2.R",
#'   across = tibble(chat = c(
#'     ellmer::chat_openai(model = "gpt-4o-mini", echo = FALSE),
#'     ellmer::chat_claude(model = "claude-3-5-sonnet-latest", echo = FALSE))
#'   ),
#'   repeats = 2
#' )
#'
#' @export
evaluate <- function(path = ".", across = tibble(), repeats = 1L, ...) {
  evaluate_impl(path = path, across = across, repeats = repeats, ...)

  invisible(results_tibble())
}

#' @rdname evaluate
#' @export
evaluate_active_file <- function(path = active_eval_file(), across = tibble(),
                                 repeats = 1L, ...) {
  evaluate_impl(path = path, across = across, repeats = repeats, ...)

  invisible(results_tibble())
}

evaluate_impl <- function(path,
                          across,
                          repeats,
                          ...,
                          reporter = NULL,
                          call = caller_env()) {
  check_data_frame(across)
  check_number_whole(repeats, min = 1, allow_infinite = FALSE, call = call)

  eval_files <- eval_files(path)

  withr::local_envvar(devtools::r_env_vars())

  if (repeats == 1 &&
      is.null(reporter) &&
      length(eval_files$eval_files) == 1 &&
      nrow(across) < 2) {
    reporter <- EvalCompactProgressReporter$new()
  } else {
    reporter <- reporter %||% EvalProgressReporter$new()
  }

  # TODO: this should just be a straight-up pmap. Source each of the eval
  # files to get a list of functions, call each of them with elements of
  # across.

  reporter$start_reporter()
  withr::defer(reporter$end_reporter())
  if (nrow(across) == 0) {
    test_eval(
      test_dir = eval_files$eval_dir,
      test_package = NULL,
      load_package = "none",
      test_paths = rep(
        file.path(eval_files$eval_dir, eval_files$eval_files),
        times = repeats
      ),
      args = list(),
      reporter = reporter
    )
  } else {
    for (i in seq_len(nrow(across))) {
      test_eval(
        test_dir = eval_files$eval_dir,
        test_package = NULL,
        load_package = "none",
        test_paths = rep(
          file.path(eval_files$eval_dir, eval_files$eval_files),
          times = repeats
        ),
        args = purrr::flatten(across[i, , drop = FALSE]),
        reporter = reporter
      )
    }
  }

  invisible(results_tibble())
}

# an anologue to `test_files_serial()` but instead of sourcing files who raise
# conditions in order to log expectation results, it evaluates functions (defined
# in those files) that raise such conditions.
test_eval <- function(test_dir,
                      test_package,
                      test_paths,
                      load_package,
                      reporter,
                      # only one row worth of `across`
                      args,
                      desc = NULL,
                      stop_on_failure = FALSE,
                      stop_on_warning = FALSE,
                      error_call = caller_env()) {
  env <- testthat::test_env()
  #reporter$start_context(env)
  env <- testthat:::test_files_setup_env(
    test_package,
    test_dir,
    load_package,
    env
  )
  testthat:::local_testing_env(env)

  reporters <- testthat:::test_files_reporter(reporter)
  testthat::with_reporter(
    reporters$multi,
    lapply(
      test_paths,
      eval_one_file,
      args = args,
      env = env,
      desc = desc,
      error_call = error_call
    ),
    start_end_reporter = FALSE
  )
}

# analogous to testthat:::test_one_file
eval_one_file <- function(path, args, env, desc = NULL, error_call = caller_env()) {
  reporter <- testthat::get_reporter()
  on.exit(testthat:::teardown_run(), add = TRUE)
  reporter$start_file(path)
  source_eval(path, args = args, reporter = reporter, env = rlang::env(env), desc = desc, error_call = error_call)
  reporter$end_context_if_started()
  reporter$end_file()
}

source_eval <- function (path, args, reporter, env, chdir = TRUE, desc = NULL,
                         wrap = TRUE, error_call = caller_env()) {
  stopifnot(file.exists(path))
  stopifnot(is.environment(env))

  path_sourced <- source(path)
  eval_fn <- path_sourced$value

  if (!is.function(eval_fn)) {
    cli::cli_abort(
      "{.arg path} must return a function when sourced.",
      call = error_call
    )
  }

  # tell the reporter which args it's currently iterating on
  # todo: need to take a cross with the formals so that there's still
  # context when default arguments are used
  reporter$start_context(args)

  withr::local_options(testthat_topenv = env, testthat_path = path)
  if (wrap) {
    invisible(
      testthat:::test_code(
        test = basename(path),
        # todo: args doesn't seem to have names
        code = rlang::call2(eval_fn, !!!args),
        env = env,
        default_reporter = StopReporter$new()
      )
    )
  }
  else {
    withCallingHandlers(invisible(eval(exprs, env)), error = function(err) {
      abort(paste0("In path: ", encodeString(path, quote = "\"")),
            parent = err, call = error_call)
    })
  }
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

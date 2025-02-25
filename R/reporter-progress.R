#' Test reporters for LLM evaluation
#'
#' @description
#' evalthat provides a number of custom testthat [testthat::Reporter]s.
#' These reporters process the results from test files, generating interactive
#' summaries and saving results to persistent files.
#'
#' `EvalProgressReporter` is designed for interactive use. Its goal is to
#' give you actionable insights to help you understand the status of your
#' code. This reporter also praises you from time-to-time if all your tests
#' pass. It's the default reporter for [test_dir()].
#'
#' `EvalCompactProgressReporter` is a minimal version of `EvalProgressReporter`
#' designed for use with single files. It's the default reporter for
#' [test_file()].
#'
#'
#' @field show_praise Whether to show praise. Will not be shown, even if `TRUE`.
#'
#' @field res_ok,res_skip,res_warn,res_fail The same as fields prefixed with
#' `n_*` in the superclass, but encoded a vector whose sum can be taken to
#' find the `n_*` values. Enables weights per-expectation.
#' @field ctxt_res_ok,ctxt_res_skip,ctxt_res_warn,ctxt_res_fail,ctxt_issues,ctxt_n The
#' same as those without the `ctxt_*` prefix, but per-context.
#' @field io The inputs and outputs flagged with [input()] and [output()].
#'
#' @name EvalReporters
#' @export
#' @family reporters
EvalProgressReporter <- R6::R6Class(
  "EvalProgressReporter",
  inherit = testthat::ProgressReporter,
  public = list(
    show_praise = FALSE,
    ctxt_issues = NULL,
    ctxt_n = 0,

    queue = list(),

    res_ok = numeric(),
    res_skip = numeric(),
    res_warn = numeric(),
    res_fail = numeric(),
    ctxt_res_ok = numeric(),
    ctxt_res_skip = numeric(),
    ctxt_res_warn = numeric(),
    ctxt_res_fail = numeric(),

    #' @description
    #' Method overwritting to always return `FALSE`, as failed evals are not
    #' a reason to stop testing in our context.
    is_full = function() {FALSE},

    #' @description
    #' Concatenated inputs and outputs flagged with `input()` and `output()`.
    io = list(),

    #' @description
    #' Concatenate inputs and outputs flagged with `input()` and `output()`.
    #'
    #' @param x The input or output.
    #' @param test The name of the test chunk.
    #' @param type One of "input" or "output".
    update_io = function(x, test, type) {
      self$io[[test]][[type]] <- x
    },

    #' @description
    #' Setup for starting a new test file.
    #'
    #' @param file File name.
    start_file = function(file) {
      self$file_name <- file
      self$ctxt_issues <- testthat:::Stack$new()
      self$ctxt_start_time <- proc.time()

      self$reset_counts()
    },

    #' @description
    #' Eval reporters' `n_*` fields are actually tracked by
    #' maintaining vectors of 1s prefixed with `res_*`. This is to accommodate
    #' a future extension where weights can be assigned to each pass/fail.
    #'
    #' This function updates the `n_*` values based on the `res_*` values.
    update_counts = function() {
      self$n_ok <- sum(self$res_ok)
      self$n_skip <- sum(self$res_skip)
      self$n_warn <- sum(self$res_warn)
      self$n_fail <- sum(self$res_fail)
      self$ctxt_n_ok <- sum(self$ctxt_res_ok)
      self$ctxt_n_skip <- sum(self$ctxt_res_skip)
      self$ctxt_n_warn <- sum(self$ctxt_res_warn)
      self$ctxt_n_fail <- sum(self$ctxt_res_fail)
    },

    #' @description
    #' Sets the values of `res_*` fields to `numeric()` and calls
    #' `update_counts()`.
    reset_counts = function() {
      self$res_ok <- numeric()
      self$res_skip <- numeric()
      self$res_warn <- numeric()
      self$res_fail <- numeric()
      self$ctxt_res_ok <- numeric()
      self$ctxt_res_skip <- numeric()
      self$ctxt_res_warn <- numeric()
      self$ctxt_res_fail <- numeric()

      self$update_counts()
    },

    #' @description
    #' Resets counters and initiates a progress bar.
    #'
    #' @param context Arguments supplied to the evaluation function, as a
    #' named list.
    start_context = function(context) {
      # todo: this is super fragile
      context <- setNames(vapply(context, maybe_str, character(1)), names(context))
      self$ctxt_name <- paste0(unname(context), collapse = ", ", recycle0 = TRUE)
      self$.context <- context
      self$ctxt_issues <- testthat:::Stack$new()

      self$ctxt_res_ok <- numeric()
      self$ctxt_res_fail <- numeric()
      self$ctxt_res_warn <- numeric()
      self$ctxt_res_skip <- numeric()

      self$ctxt_n <- 0L
      self$ctxt_start_time <- proc.time()
      self$show_status()
    },

    #' @description
    #' Show the header tabulating successes and failures.
    show_header = function() {
      self$cat_line(
        "  ",
        colorize("PASS", "success"),
        colorize(" FAIL", "failure"),
        " | ", "Context"
      )
    },

    #' @description
    #' Tabulate successes and failures in the current context.
    #'
    #' @param complete Logical.
    #' @param time Ignored. TODO: remove this?
    #' @param pad Logical.
    show_status = function(complete = FALSE, time = 0, pad = FALSE) {
      data <- self$status_data()
      if (!complete) {
        if (!self$should_update()) {
          return()
        }
        status <- spinner(self$frames, data$n)
      } else {
        status <- cli::format_inline("\u2713")
      }

      col_format <- function(n, type) {
        if (n == 0) {
          if (type == "skip") {
            "  "
          } else {
            " "
          }
        } else {
          if (type == "skip") {
            colorize(sprintf("%2d", n), type)
          } else {
            colorize(n, type)
          }
        }
      }

      context <- ansi_collapse_context(self$ctxt_name, self$width - 40)
      message <- paste0(
        status, " ",
        sprintf("%4d", self$n_ok), " ",
        sprintf("%4d", self$n_fail),
        " | ", file_name_to_context(self$file_name), context
      )

      if (complete) {
        message <- paste0(
          message,
          " (",
          color_gradient(self$n_ok / max(self$n_fail + self$n_ok, 1)),
          ")"
        )
      }

      if (pad) {
        message <- strpad(message, self$width)
        message <- cli::ansi_substr(message, 1, self$width)
      }

      if (!complete) {
        message <- strpad(message, self$width)
        self$cat_tight(self$cr(), message)
      } else {
        self$cat_line(self$cr(), message)
      }
    },

    #' @description
    #' Teardown following the test run.
    end_context = function() {
      time <- proc.time() - self$ctxt_start_time
      self$last_update <- NULL

      self$save_results()

      self$show_status(complete = TRUE, time = time[[3]])
    },

    #' @description
    #' Situates the evaluation results in a tibble and saves it to the
    #' file `eval_file_name/timestamp.rds` using [qs::qread()]. Read individual
    #' results with [qs::qsave()].
    save_results = function() {
      subdir <- file_name_to_context(self$file_name)
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

      results_dir <- file.path(dirname(self$file_name), "_results", subdir)
      if (!dir.exists(results_dir)) {
        dir.create(results_dir, recursive = TRUE)
      }

      qs::qsave(
        x = self$result_summary(timestamp),
        file = file.path(results_dir, paste0(timestamp, ".rds"))
      )
    },

    #' Situate evaluation results in a tibble.
    #'
    #' @param timestamp DTTM as `format(Sys.time(), "%Y%m%d_%H%M%S")`.
    result_summary = function(timestamp) {
      res <-
        tibble::tibble(
          evaluating = list(self$.context),
          n_fail = self$n_fail,
          n_pass = self$n_ok,
          timestamp = timestamp,
          file_hash = hash_file(self$file_name),
          io = list(self$io),
          problems = list(self$problems$as_list())
        )

      if (!identical(self$queue, list())) {
        # has results that need grading
        res <- dplyr::mutate(res, queue = list(self$queue), .before = n_fail)
      } else {
        res <-
          res %>%
          dplyr::mutate(
            pct = n_pass * 100 / max(n_fail + n_pass, 1),
            .before = n_fail
          )
      }

      evalthat_env$last_result <- c(evalthat_env$last_result, list(res))

      res
    },

    #' @description
    #' Append a given `expect_*()` or `grade_*()` result to the current context.
    #'
    #' @param context Context from [evaluating()].
    #' @param test The name of the test block.
    #' @param result Result.
    add_result = function(context, test, result) {
      self$ctxt_n <- self$ctxt_n + 1L

      if (expectation_broken(result)) {
        self$res_fail <- c(self$res_fail, 1)
        self$ctxt_res_fail <- c(self$ctxt_res_fail, 1)
        result$trace <- NULL
        self$ctxt_issues$push(result)
        self$problems$push(result)
      } else if (expectation_skip(result)) {
        self$res_skip <- c(self$res_skip, 1)
        self$ctxt_res_skip <- c(self$ctxt_res_skip, 1)
        self$skips$push(result)
      } else if (expectation_warning(result)) {
        self$res_warn <- c(self$res_warn, 1)
        self$ctxt_res_warn <- c(self$ctxt_res_warn, 1)
        self$ctxt_issues$push(result)
      } else {
        self$res_ok <- c(self$res_ok, 1)
        self$ctxt_res_ok <- c(self$ctxt_res_ok, 1)
      }

      # testthat hardcodes its multi-reporter as testthat::MultiReporter,
      # so we can't add a custom field to the multi-reporter for `$update_io()`.
      # instead, just `update_io()` each time a result is added (possibly
      # overwriting previous entries).
      evalthatenv <- env_get(ns_env("evalthat"), "evalthat_env")
      self$update_io(evalthatenv$input, test, "input")
      self$update_io(evalthatenv$output, test, "output")

      self$update_counts()
      self$show_status()
    },

    cache_for_grading = function(target) {
      evalthatenv <- env_get(ns_env("evalthat"), "evalthat_env")

      self$queue <- c(
        self$queue,
        list(list(
          input = evalthatenv$input,
          output = evalthatenv$output,
          target = target
        ))
      )
    },

    #' @description
    #' Start the current reporter.
    start_reporter = function() {
      evalthat_env$last_result <- list()
      self$start_time <- proc.time()
      self$show_header()
    },

    #' @description
    #' Tear down the current reporter.
    end_reporter = function() {
      colour_if <- function(n, type) {
        colorize(n, if (n == 0) "success" else type)
      }

      time <- proc.time() - self$start_time
      if (time[[3]] > self$min_time) {
        self$cat_line("Duration: ", sprintf("%.1f s", time[[3]]), col = "cyan")
      }
    },

    #' @description
    #' Overrides the superclass' method to end the context when the file ends
    #' since files are reran when running evals.
    end_file = function() {
      self$end_context()
    }
  )
)

testthat_max_fails <- function() {
  val <- getOption("testthat.progress.max_fails")

  if (is.null(val)) {
    env <- Sys.getenv("TESTTHAT_MAX_FAILS")
    val <- if (!identical(env, "")) as.numeric(env) else 10
  }
  val
}

#' @export
#' @rdname EvalReporters
EvalCompactProgressReporter <- R6::R6Class(
  "EvalCompactProgressReporter",
  inherit = EvalProgressReporter,
  public = list(
    #' @description
    #' Sets minimum time to infinity.
    #'
    #' @param min_time A numeric. Defaults to `Inf`.
    #' @param ... Passed on to `super$initialize()`.
    initialize = function(min_time = Inf, ...) {
      super$initialize(min_time = min_time, ...)
    },

    #' @description
    #' Setup for a single file.
    #'
    #' @param name File name.
    start_file = function(name) {
      self$file_name <- name
      self$ctxt_issues <- testthat:::Stack$new()
      self$ctxt_start_time <- proc.time()
    },

    #' @description
    #' Setup.
    start_reporter = function() {
      evalthat_env$last_result <- list()
    },

    #' @description
    #' Teardown.
    end_reporter = function() {
    },

    #' @description
    #' Show current status.
    #'
    #' @param complete Ignored.
    #' @param time Ignored--included for compatibility with [EvalProgressReporter].
    show_status = function(complete = NULL, time = NULL) {
      self$local_user_output()
      status <- summary_line(
        self$n_fail,
        self$n_warn,
        self$n_skip,
        self$n_ok,
        context = file_name_to_context(self$file_name),
        width = self$width
      )
      self$cat_tight(self$cr(), status)
    }
  )
)

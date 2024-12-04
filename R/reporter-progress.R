#' Test reporter: interactive progress bar of errors.
#'
#' @description
#' `EvalProgressReporter` is designed for interactive use. Its goal is to
#' give you actionable insights to help you understand the status of your
#' code. This reporter also praises you from time-to-time if all your tests
#' pass. It's the default reporter for [test_dir()].
#'
#' `EvalCompactProgressReporter` is a minimal version of `EvalProgressReporter`
#' designed for use with single files. It's the default reporter for
#' [test_file()].
#'
#' @export
#' @family reporters
EvalProgressReporter <- R6::R6Class(
  "EvalProgressReporter",
  inherit = testthat::ProgressReporter,
  public = list(
    show_praise = FALSE,
    res_ok = numeric(),
    res_skip = numeric(),
    res_warn = numeric(),
    res_fail = numeric(),
    dynamic = FALSE,
    ctxt_issues = NULL,
    ctxt_n = 0,
    ctxt_res_ok = numeric(),
    ctxt_res_skip = numeric(),
    ctxt_res_warn = numeric(),
    ctxt_res_fail = numeric(),
    evaluating_context = list(),

    # many failures are not a reason to stop in this context--will just
    # result in a poor eval
    is_full = function() {FALSE},

    # inputs and outputs, flagged with `eval_input()` and `eval_output()`
    io = list(),

    start_file = function(file) {
      self$file_name <- file
      self$ctxt_issues <- testthat:::Stack$new()
      self$ctxt_start_time <- proc.time()

      self$reset_counts()
    },

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

    start_context = function(context) {
      self$evaluating_context <- context

      self$ctxt_name <- context
      self$ctxt_issues <- testthat:::Stack$new()

      self$ctxt_res_ok <- numeric()
      self$ctxt_res_fail <- numeric()
      self$ctxt_res_warn <- numeric()
      self$ctxt_res_skip <- numeric()

      self$ctxt_n <- 0L
      self$ctxt_start_time <- proc.time()
      self$show_status()
    },

    show_header = function() {
      self$cat_line(
        "  ",
        colorize("PASS", "success"),
        colorize(" FAIL", "failure"),
        " | ", "Context"
      )
    },

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

      message <- paste0(
        status, " ",
        sprintf("%4d", self$n_ok), " ",
        sprintf("%4d", self$n_fail),
        " | ", cli::ansi_collapse(data$name, sep2 = " for ")
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

    end_context = function(context) {
      time <- proc.time() - self$ctxt_start_time
      self$last_update <- NULL

      # context with no expectation = automatic file context in file
      # that also has manual contexts
      if (self$ctxt_n == 0) {
        return()
      }

      self$save_results()

      self$show_status(complete = TRUE, time = time[[3]])
    },

    save_results = function() {
      # remove the file extension and test- prefix
      subdir <- sub("\\.([^.]*)$", "", self$file_name)
      subdir <- sub("^test-", "", subdir)
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

      results_dir <- file.path("_results", subdir)
      if (!dir.exists(results_dir)) {
        dir.create(results_dir, recursive = TRUE)
      }

      qs::qsave(
        x = self$result_summary(timestamp),
        file = file.path(results_dir, paste0(timestamp, ".rds"))
      )
    },

    result_summary = function(timestamp) {
      tibble::tibble(
        model = self$evaluating_context$model,
        task = self$evaluating_context$task,
        context = list(self$evaluating_context[
          !names(self$evaluating_context) %in% c("model", "task")
        ]),
        pct = self$n_ok * 100 / max(self$n_fail + self$n_ok, 1),
        n_fail = self$n_fail,
        n_ok = self$n_ok,
        timestamp = timestamp,
        file_hash = hash_file(self$file_name),
        io = list(self$io),
        problems = list(self$problems$as_list())
      )
    },

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

    end_reporter = function() {
      self$cat_line()

      colour_if <- function(n, type) {
        colorize(n, if (n == 0) "success" else type)
      }

      time <- proc.time() - self$start_time
      if (time[[3]] > self$min_time) {
        self$cat_line("Duration: ", sprintf("%.1f s", time[[3]]), col = "cyan")
        self$cat_line()
      }

      if (self$problems$size() > 0) {
        self$cat_tight(cli::format_inline(
          "See {.field problems} in {.fun results_read} for more information on failures."
        ))
      }

      self$cat_line()
    },

    update_io = function(x, test, type) {
      self$io[[test]][[type]] <- x
    },

    report_issues = function(issues) {
      if (issues$size() > 0) {
        self$rule()

        issues <- issues$as_list()
        summary <- vapply(issues, issue_summary, FUN.VALUE = character(1))
        self$cat_tight(paste(summary, collapse = "\n\n"))

        self$cat_line()
        self$rule()
      }
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
#' @rdname ProgressReporter
EvalCompactProgressReporter <- R6::R6Class(
  "EvalCompactProgressReporter",
  inherit = EvalProgressReporter,
  public = list(
    initialize = function(min_time = Inf, ...) {
      super$initialize(min_time = min_time, ...)
    },
    start_file = function(name) {
      if (!self$rstudio) {
        self$cat_line()
        self$rule(cli::style_bold(paste0("Testing ", name)), line = 2)
      }
      self$file_name <- name
      self$ctxt_issues <- testthat:::Stack$new()
      self$ctxt_start_time <- proc.time()
    },
    start_reporter = function(context) {
    },
    start_context = function(context) {
      self$evaluating_context <- context
      self$cat_line(format_context(context))
    },
    end_context = function(context) {
      if (self$ctxt_issues$size() == 0) {
        return()
      }

      self$cat_line()
      self$cat_line()

      issues <- self$ctxt_issues$as_list()
      summary <- vapply(issues, issue_summary, rule = TRUE,
                        FUN.VALUE = character(1)
      )
      self$cat_tight(paste(summary, collapse = "\n\n"))

      self$cat_line()
    },
    end_reporter = function() {
      had_feedback <- self$n_fail > 0 || self$n_warn > 0

      if (self$n_skip > 0) {
        if (!had_feedback) {
          self$cat_line()
        }
        self$cat_line()
      }
      self$save_results()

      if (had_feedback) {
        self$cat_line()
        self$show_status()
        self$cat_line()
      } else if (self$n_skip == 0 && !self$rstudio) {
        self$cat_line(cli::style_bold(" Done!"))
      }
    },
    show_status = function(complete = NULL) {
      self$local_user_output()
      status <- summary_line(self$n_fail, self$n_warn, self$n_skip, self$n_ok)
      self$cat_tight(self$cr(), status)
    }
  )
)

# helpers -----------------------------------------------------------------

spinner <- function(frames, i) {
  frames[((i - 1) %% length(frames)) + 1]
}

issue_header <- function(x, pad = FALSE) {
  type <- expectation_type(x)
  if (has_color()) {
    type <- colorize(first_upper(type), type)
  } else {
    type <- first_upper(type)
  }
  if (pad) {
    type <- strpad(type, 7)
  }

  paste0(type, expectation_location(x, " (", ")"), ": ", x$test)
}

issue_summary <- function(x, rule = FALSE) {
  header <- cli::style_bold(issue_header(x))
  if (rule) {
    # Don't truncate long test names
    width <- max(cli::ansi_nchar(header) + 6, getOption("width"))
    header <- cli::rule(header, width = width)
  }

  paste0(header, "\n", format(x))
}

strpad <- function(x, width = cli::console_width()) {
  n <- pmax(0, width - cli::ansi_nchar(x))
  paste0(x, strrep(" ", n))
}

first_upper <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

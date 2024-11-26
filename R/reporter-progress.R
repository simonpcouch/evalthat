#' Test reporter: interactive progress bar of errors.
#'
#' @description
#' `EvalProgressReporter` is designed for interactive use. Its goal is to
#' give you actionable insights to help you understand the status of your
#' code. This reporter also praises you from time-to-time if all your tests
#' pass. It's the default reporter for [test_dir()].
#'
#' `EvalParallelProgressReporter` is very similar to `EvalProgressReporter`, but
#' works better for packages that want parallel tests.
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
    show_praise = TRUE,
    min_time = 1,
    start_time = NULL,
    last_update = NULL,
    update_interval = NULL,
    skips = NULL,
    problems = NULL,
    max_fail = NULL,
    res_ok = numeric(),
    res_skip = numeric(),
    res_warn = numeric(),
    res_fail = numeric(),
    frames = NULL,
    dynamic = FALSE,
    ctxt_start_time = NULL,
    ctxt_issues = NULL,
    ctxt_n = 0,
    ctxt_res_ok = numeric(),
    ctxt_res_skip = numeric(),
    ctxt_res_warn = numeric(),
    ctxt_res_fail = numeric(),
    ctxt_name = "",
    file_name = "",
    n_ok = 0,
    n_skip = 0,
    n_warn = 0,
    n_fail = 0,
    ctxt_n_ok = 0,
    ctxt_n_skip = 0,
    ctxt_n_warn = 0,
    ctxt_n_fail = 0,

    start_file = function(file) {
      self$file_name <- file
      self$ctxt_issues <- testthat:::Stack$new()
      self$ctxt_start_time <- proc.time()

      #context_start_file(self$file_name)
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
        colorize(cli::symbol$tick, "success"), " | ",
        colorize("PASS", "success"),
        colorize(" FAIL", "failure"),
        " | ", "Context"
      )
    },

    show_status = function(complete = FALSE, time = 0, pad = FALSE) {
      data <- self$status_data()
      if (complete) {
        if (self$n_fail > 0) {
          status <- cli::col_red(cli::symbol$cross)
        } else {
          status <- cli::col_green(cli::symbol$tick)
        }
      } else {
        if (!self$should_update()) {
          return()
        }
        status <- spinner(self$frames, data$n)
        if (self$n_fail > 0) {
          status <- colorize(status, "failure")
        } else if (self$n_warn > 0) {
          status <- colorize(status, "warning")
        }
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
        status, " | ",
        sprintf("%4d", self$n_ok), " ",
        sprintf("%4d", self$n_fail),
        " | ", data$name
      )

      if (complete && time > self$min_time) {
        message <- paste0(
          message,
          cli::col_grey(sprintf(" [%.1fs]", time))
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

      self$show_status(complete = TRUE, time = time[[3]])
      # TODO: this previously reported issues during testing, but felt
      # excessive since failures are also reported after
      #self$report_issues(self$ctxt_issues)

      if (self$is_full()) {
        snapshotter <- get_snapshotter()
        if (!is.null(snapshotter)) {
          snapshotter$end_file()
        }

        stop_reporter(c(
          "Maximum number of failures exceeded; quitting at end of file.",
          i = "Increase this number with (e.g.) {.run testthat::set_max_fails(Inf)}"
        ))
      }
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

      self$update_counts()
      self$show_status()
    },

    end_reporter = function() {
      self$cat_line()

      colour_if <- function(n, type) {
        colorize(n, if (n == 0) "success" else type)
      }

      self$rule(cli::style_bold("Results"), line = 2)
      time <- proc.time() - self$start_time
      if (time[[3]] > self$min_time) {
        self$cat_line("Duration: ", sprintf("%.1f s", time[[3]]), col = "cyan")
        self$cat_line()
      }

      if (self$problems$size() > 0) {
        problems <- self$problems$as_list()
        self$rule("Failed evals", line = 1)
        for (problem in problems) {
          self$cat_line(issue_summary(problem))
          self$cat_line()
        }
      }

      status <- summary_line(self$n_fail, self$n_warn, self$n_skip, self$n_ok)
      self$cat_line("")
      self$cat_line(status)

      if (self$is_full()) {
        self$rule("Terminated early", line = 2)
      }

      if (!self$show_praise || stats::runif(1) > 0.1) {
        return()
      }

      self$cat_line()
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
      self$cat_line(context)
    },
    end_context = function(context) {
      if (self$ctxt_issues$size() == 0) {
        return()
      }

      browser()

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

      if (had_feedback) {
        self$cat_line()
        self$show_status()
        self$cat_line()
      } else if (self$is_full()) {
        self$cat_line(" Terminated early")
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

# parallel progres reporter -----------------------------------------------

#' @export
#' @rdname ProgressReporter

EvalParallelProgressReporter <- R6::R6Class(
  "EvalParallelProgressReporter",
  inherit = testthat::ProgressReporter,
  public = list(
    files = list(),
    spin_frame = 0L,
    is_rstudio = FALSE,
    initialize = function(...) {
      super$initialize(...)
      self$capabilities$parallel_support <- TRUE
      self$capabilities$parallel_updates <- TRUE
      self$update_interval <- 0.05
      self$is_rstudio <- Sys.getenv("RSTUDIO", "") == "1"
    },
    start_file = function(file)  {
      if (! file %in% names(self$files)) {
        self$files[[file]] <- list(
          issues = testthat:::Stack$new(),
          n_fail = 0L,
          n_skip = 0L,
          n_warn = 0L,
          n_ok = 0L,
          name = context_name(file),
          start_time = proc.time()
        )
      }
      self$file_name <- file
    },
    start_context = function(context) {
      # we'll just silently ignore this
    },
    end_context = function(context) {
      # we'll just silently ignore this
    },
    end_file = function() {
      fsts <- self$files[[self$file_name]]
      time <- proc.time() - fsts$start_time

      # Workaround for https://github.com/rstudio/rstudio/issues/7649
      if (self$is_rstudio) {
        self$cat_tight(strpad(self$cr(), self$width + 1)) # +1 for \r
      }
      self$show_status(complete = TRUE, time = time[[3]], pad = TRUE)
      self$report_issues(fsts$issues)

      self$files[[self$file_name]] <- NULL
      if (length(self$files)) self$update(force = TRUE)
    },
    end_reporter = function() {
      self$cat_tight(self$cr(), strpad("", self$width))
      super$end_reporter()
    },
    show_header = function() {
      super$show_header()
      self$update(force = TRUE)
    },
    status_data = function() {
      self$files[[self$file_name]]
    },
    add_result = function(context, test, result) {
      self$ctxt_n <- self$ctxt_n + 1L
      file <- self$file_name
      if (expectation_broken(result)) {
        self$n_fail <- self$n_fail + 1
        self$files[[file]]$n_fail <- self$files[[file]]$n_fail + 1L
        self$files[[file]]$issues$push(result)
        self$problems$push(result)
      } else if (expectation_skip(result)) {
        self$n_skip <- self$n_skip + 1
        self$files[[file]]$n_skip <- self$files[[file]]$n_skip + 1L
        self$skips$push(result)
      } else if (expectation_warning(result)) {
        self$n_warn <- self$n_warn + 1
        self$files[[file]]$n_warn <- self$files[[file]]$n_warn + 1L
        self$files[[file]]$issues$push(result)
      } else {
        self$n_ok <- self$n_ok + 1
        self$files[[file]]$n_ok <- self$files[[file]]$n_ok + 1
      }
    },
    update = function(force = FALSE) {
      if (!force && !self$should_update()) return()
      self$spin_frame <- self$spin_frame + 1L
      status <- spinner(self$frames, self$spin_frame)

      message <- paste(
        status,
        summary_line(self$n_fail, self$n_warn, self$n_skip, self$n_ok),
        if (length(self$files) > 0) "@" else "Starting up...",
        paste(context_name(names(self$files)), collapse = ", ")
      )
      message <- strpad(message, self$width)
      message <- cli::ansi_substr(message, 1, self$width)
      self$cat_tight(self$cr(), message)
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

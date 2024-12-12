# TODO: EvalProgressReporter$show_status may need to transition to this
summary_line <- function(n_fail, n_warn, n_skip, n_pass, context, width) {
  colorize_if <- function(text, color, cond) {
    if (cond) colorize(text, color) else text
  }

  prop <- n_pass / max(n_fail + n_pass, 1)
  if (prop == 0) {
    suffix <- ""
  } else {
    suffix <- paste0(": ", color_gradient(prop))
  }

  strpad(
    paste0(
      "",
      colorize_if("PASS", "success", n_pass > 0), " ", n_pass, " | ",
      colorize_if("FAIL", "failure", n_fail > 0), " ", n_fail,
      suffix,
      ansi_collapse_context(context, width = 20)
    ),
    width = width
  )

}

color_gradient <- function(prop) {
  # TODO: check_number_decimal(prop, min = 0, max = 1)?

  if (prop <= 0.5) {
    # transition from red to yellow
    red <- 1
    green <- 2 * prop
  } else {
    # transition from yellow to green
    red <- 2 - 2 * prop
    green <- 1
  }

  # make colors slightly darker by setting `maxColorValue` to twice max r and g
  hex_color <- rgb(red, green, b = 0, maxColorValue = 2)

  cli::make_ansi_style(hex_color)(sprintf("%.1f%%", prop * 100))
}

# remove directory path, extension, and `test-` prefix
file_name_to_context <- function(file_name) {
  res <- basename(file_name)
  res <- sub("\\.([^.]*)$", "", res)
  res <- sub("^test-", "", res)
  res
}

# copied from testthat ---------------------------------------------------------
colorize <- function(text, as = c("success", "skip", "warning", "failure", "error")) {
  if (has_color()) {
    unclass(cli::make_ansi_style(testthat_style(as))(text))
  } else {
    text
  }
}

has_color <- function() {
  isTRUE(getOption("testthat.use_colors", TRUE)) &&
    cli::num_ansi_colors() > 1
}

testthat_style <- function(type = c("success", "skip", "warning", "failure", "error")) {
  type <- match.arg(type)

  c(
    success = "green",
    skip = "blue",
    warning = "magenta",
    failure = "orange",
    error = "orange"
  )[[type]]
}

issue_summary <- function(x, rule = FALSE) {
  header <- cli::style_bold(testthat:::issue_header(x))
  if (rule) {
    header <- cli::rule(header, width = max(cli::ansi_nchar(header) + 6, 80))
  }
  paste0(header, "\n", format(x))
}

expectation_type <- function(exp) {
  stopifnot(is.expectation(exp))
  gsub("^expectation_", "", class(exp)[[1]])
}

expectation_success <- function(exp) expectation_type(exp) == "success"
expectation_failure <- function(exp) expectation_type(exp) == "failure"
expectation_error   <- function(exp) expectation_type(exp) == "error"
expectation_skip    <- function(exp) expectation_type(exp) == "skip"
expectation_warning <- function(exp) expectation_type(exp) == "warning"
expectation_broken  <- function(exp) expectation_failure(exp) || expectation_error(exp)
expectation_ok      <- function(exp) expectation_type(exp) %in% c("success", "warning")

expectation_location <- function(x, prefix = "", suffix = "") {
  srcref <- x$srcref
  if (!inherits(srcref, "srcref")) {
    return("")
  }

  filename <- attr(srcref, "srcfile")$filename
  cli::format_inline("{prefix}{.file {filename}:{srcref[1]}:{srcref[2]}}{suffix}")
}


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

ansi_collapse_context <- function(ctxt, width) {
  res <- unlist(ctxt[vapply(ctxt, is.character, logical(1))])

  if (length(res) == 0) {return("")}
  if (length(res) == 1) {return(paste0(" [", unname(res), "]"))}

  res <- cli::ansi_collapse(
    res,
    sep = "/",
    sep2 = "/",
    last = "/",
    width = width,
    style = "head"
  )

  paste0(" [", res, "]")
}

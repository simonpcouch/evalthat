skip_if_not_installed("pal")
skip_if_offline()
skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))

cli_pal <- pal::.init_pal("cli")

evaluating(
  cli_pal$.__enclos_env__$private$Chat$.__enclos_env__$private$provider@model,
  "translating to cli"
)

test_that("cli pal works for trivial function call", {
  cli_pal <- pal::.init_pal("cli")
  input <- "rlang::abort(\"some message\")"
  output <- cli_pal$chat(input)

  expect_r_code(output)
  expect_match(output, "cli::cli_abort")
  expect_match(output, "some message")
})

test_that("cli pal works for one-liner", {
  cli_pal <- pal::.init_pal("cli")
  input <- "rlang::abort(\"`save_pred` can only be used if the initial results saved predictions.\")"
  output <- cli_pal$chat(input)

  expect_r_code(output)
  expect_match(output, "cli::cli_abort")

  # doesn't tamper with the majority of the message
  expect_match(output, "can only be used if the initial results saved predictions")

  # incorporates inline markup
  expect_match(output, "{.arg save_pred}", fixed = TRUE)
})

test_that("cli pal collapses ad-hoc enumeration", {
  cli_pal <- pal::.init_pal("cli")
  input <- "
    extra_grid_params <- glue::single_quote(extra_grid_params)
    extra_grid_params <- glue::glue_collapse(extra_grid_params, sep = \", \")

    msg <- glue::glue(
      \"The provided `grid` has the following parameter columns that have \",
      \"not been marked for tuning by `tune()`: {extra_grid_params}.\"
    )

    rlang::abort(msg)
  "
  output <- cli_pal$chat(input)

  expect_r_code(output)
  expect_match(output, "cli::cli_abort", fixed = TRUE)

  # inlines ad-hoc enumeration in message
  expect_match(output, "^cli")

  # incorporates inline markup
  expect_match(output, "{.arg grid}", fixed = TRUE)
  expect_match(output, "{.fn tune}", fixed = TRUE)
  expect_match(output, "{extra_grid_params}", fixed = TRUE)

  # correctly pluralizes
  expect_match(output, "column{?s}", fixed = TRUE)
  expect_match(output, "{?has/have}", fixed = TRUE)

  # line breaks in reasonable places
  expect_match(output, "cli_abort(\n", fixed = TRUE)
  expect_match(output, "\n)$")
})

test_that("cli pal collapses ad-hoc enumeration (pt. 2)", {
  cli_pal <- pal::.init_pal("cli")
  input <- "
    rlang::abort(paste0(
      \"The workflow has arguments to be tuned that are missing some \",
      \"parameter objects: \",
      paste0(\"'\", pset$id[!params], \"'\", collapse = \", \")
    ))
  "
  output <- cli_pal$chat(input)

  expect_r_code(output)
  expect_match(output, "cli::cli_abort", fixed = TRUE)

  # inlines ad-hoc enumeration in message
  expect_match(output, "^cli")

  # incorporates inline markup
  expect_match(output, "{.arg {pset$id[!params]}}", fixed = TRUE)

  # correctly pluralizes
  expect_match(output, "object{?s}", fixed = TRUE)
  expect_match(output, "{?is/are}", fixed = TRUE)

  # line breaks in reasonable places
  expect_match(output, "cli_abort(\n", fixed = TRUE)
  expect_match(output, "\n)$")
})

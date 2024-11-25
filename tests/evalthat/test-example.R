test_that("example works for some input", {
  # input <- some_input_flag_fn("some text")
  # res <- some_output_flag_fn(clipal$chat(input))
  input <- "rlang::abort(\"some message\")"
  output <- "cli::cli_abort(\"some message\")"

  expect_syntactically_valid(output)
  expect_match(output, "cli::cli_abort")
  expect_match(output, "some message")
})

test_that("example works for another input", {
  # input <- some_input_flag_fn("some text")
  # res <- some_output_flag_fn(clipal$chat(input))
  input <- "rlang::abort(\"another message\")"
  output <- "cli::cli_abort(\"another message\")"

  expect_syntactically_valid(output)
  expect_false(TRUE)
  Sys.sleep(.2)
  expect_false(TRUE)
  Sys.sleep(.2)
  expect_false(TRUE)
  Sys.sleep(.2)
  expect_false(TRUE)
  Sys.sleep(.2)
  expect_match(output, "cli::cli_abort")
  expect_match(output, "another message")
})

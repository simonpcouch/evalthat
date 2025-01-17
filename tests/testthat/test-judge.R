skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))

test_that("judges works", {
  library(ellmer)

  single <- judges(gpt4o = chat_openai(model = "gpt-4o"))

  expect_s3_class(single, "judges")
  expect_snapshot(single)

  multiple <-
    judges(gpt4o = chat_openai(model = "gpt-4o"), claude = chat_claude())

  expect_s3_class(multiple, "judges")
  expect_snapshot(multiple)
})

test_that("inherits names from expressions", {
  res <-
    judges(chat_openai(model = "gpt-4o"), chat_claude())

  expect_s3_class(res, "judges")
  expect_snapshot(res)
})

test_that("mixed naming works", {
  res <-
    judges(gpt4o = chat_openai(model = "gpt-4o"), chat_claude())

  expect_s3_class(res, "judges")
  expect_snapshot(res)
})

test_that("fails informatively with non-Chat input", {
  not_chat <- list()
  expect_snapshot(judges(not_chat), error = TRUE)
  expect_snapshot(judges(x = not_chat), error = TRUE)
})


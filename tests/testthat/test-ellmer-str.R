test_that("str.Chat works", {
  expect_snapshot(str(ellmer::chat_claude()))
  expect_snapshot(str(ellmer::chat_claude(api_args = list(temperature = .7))))
  expect_snapshot(str(ellmer::chat_claude(model = "claude-3-5-sonnet-latest")))
  expect_snapshot(str(ellmer::chat_claude(
    model = "claude-3-5-sonnet-latest",
    api_args = list(temperature = .7)
  )))

  expect_snapshot(str(ellmer::chat_openai()))
  expect_snapshot(str(ellmer::chat_openai(model = "gpt-4o-mini")))
})

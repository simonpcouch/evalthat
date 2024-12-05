skip_if_offline()

if (is.null(getOption("chat_fn"))) {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  chat_fn <- elmer::chat_claude
  model <- "claude-3-5-sonnet-latest"
} else {
  chat_fn <- getOption("chat_fn")
  model <- getOption("model")
}

evaluating(
  chat_fn = "TODO-idk",
  model = getOption("model")
)

test_that("model can write ggplot2 code for a basic histogram", {
  chat <- chat_fn(
    model = model,
    system_prompt = "When asked a question about R code, reply with only the
                     code needed to answer the question. No exposition, no
                     backticks."
  )

  input <- input("Could you please write ggplot2 code to make a histogram of
                  the mpg variable from mtcars?")
  output <- output(chat$chat(input))

  # syntactically valid
  expect_r_code(output)

  # correct
  expect_match(output, "mtcars", fixed = TRUE)
  expect_match(output, "ggplot(", fixed = TRUE)
  expect_match(output, "aes(x = mpg", fixed = TRUE)
  expect_match(output, "geom_histogram(", fixed = TRUE)
})

test_that("model can convert code from base R `plot()`", {
  chat <- chat_fn(
    model = model,
    system_prompt = "When asked a question about R code, reply with only the
                     code needed to answer the question. No exposition, no
                     backticks."
  )

  input <- input("Convert this code to use ggplot2: `boxplot(len ~ supp, data = ToothGrowth)`")
  output <- output(chat$chat(input))

  # syntactically valid
  expect_r_code(output)

  # correctness
  expect_match(output, "ToothGrowth", fixed = TRUE)
  expect_match(output, "ggplot(", fixed = TRUE)
  expect_match(output, "aes(", fixed = TRUE)
  expect_match(output, "x = supp", fixed = TRUE)
  expect_match(output, "y = len", fixed = TRUE)
  expect_match(output, "geom_boxplot(", fixed = TRUE)

  # doesn't use incorrect / unneeded operators
  expect_no_match(output, "~", fixed = TRUE)
  expect_no_match(output, "$", fixed = TRUE)
})

test_that("model can convert from stacked to dodged bars", {
  chat <- chat_fn(
    model = model,
    system_prompt = "When asked a question about R code, reply with only the
                     code needed to answer the question. No exposition, no
                     backticks."
  )

  input <- input(
    "Make the bars side-by-side rather than stacked on top of each other:
     `ggplot(mtcars) + aes(x = cyl, fill = factor(vs)) + geom_bar()`
    "
  )
  output <- output(chat$chat(input))

  # syntactically valid
  expect_r_code(output)

  # leaves existing code as-is
  expect_match(output, "aes(x = cyl, fill = factor(vs))", fixed = TRUE)
  expect_match(output, "ggplot(mtcars)", fixed = TRUE)

  # correctness
  expect_match(output, "geom_bar(position = ", fixed = TRUE)
  expect_match(output, "dodge")
})

test_that("model can decrease bar width", {
  chat <- chat_fn(
    model = model,
    system_prompt = "When asked a question about R code, reply with only the
                     code needed to answer the question. No exposition, no
                     backticks."
  )

  input <- input(
    "Make the bars skinnier:
     `ggplot(mtcars) + aes(x = cyl, fill = factor(vs)) + geom_bar()`
    "
  )
  output <- output(chat$chat(input))

  # syntactically valid
  expect_r_code(output)

  # leaves existing code as-is
  expect_match(output, "aes(x = cyl, fill = factor(vs))", fixed = TRUE)
  expect_match(output, "ggplot(mtcars)", fixed = TRUE)

  # correctness
  expect_match(output, "geom_bar(width = ", fixed = TRUE)
})

test_that("model can add means to a boxplot", {
  chat <- chat_fn(
    model = model,
    system_prompt = "When asked a question about R code, reply with only the
                     code needed to answer the question. No exposition, no
                     backticks."
  )

  input <- input(
    "Add a square in each box to show the mean:
     `ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + geom_boxplot()`
    "
  )
  output <- output(chat$chat(input))

  # syntactically valid
  expect_r_code(output)

  # leaves existing code as-is
  expect_match(output, "ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +", fixed = TRUE)
  expect_match(output, "geom_boxplot() +", fixed = TRUE)

  # correctness
  expect_match(output, "stat_summary(", fixed = TRUE)
  expect_match(output, "mean", fixed = TRUE)
  expect_match(output, "shape =", fixed = TRUE)
})

test_that("model can move a legend", {
  chat <- chat_fn(
    model = model,
    system_prompt = "When asked a question about R code, reply with only the
                     code needed to answer the question. No exposition, no
                     backticks."
  )

  input <- input(
    "Move the legend to the top left, slightly inside the grey pane:
     `ggplot(mtcars) + aes(x = cyl, fill = factor(vs)) + geom_bar()`
    "
  )
  output <- output(chat$chat(input))

  # syntactically valid
  expect_r_code(output)

  # leaves existing code as-is
  expect_match(output, "aes(x = cyl, fill = factor(vs))", fixed = TRUE)
  expect_match(output, "ggplot(mtcars)", fixed = TRUE)

  # correctness
  # may either be legend.position or legend.position.inside
  expect_match(output, "theme(legend.position", fixed = TRUE)

  # this one probably needs more involved eval
  expect_match(output, "c(", fixed = TRUE)
})

test_that("model can use subscript in axis label", {
  chat <- chat_fn(
    model = model,
    system_prompt = "When asked a question about R code, reply with only the
                     code needed to answer the question. No exposition, no
                     backticks."
  )

  input <- input(
    "Make the 2 a subscript in the label:
     `ggplot(CO2) + aes(x = uptake) + geom_histogram() + labs(x = 'CO_2 Uptake')`
    "
  )
  output <- output(chat$chat(input))

  # syntactically valid
  expect_r_code(output)

  # leaves existing code as-is
  expect_match(output, "ggplot(CO2) +", fixed = TRUE)
  expect_match(output, "aes(x = uptake) +", fixed = TRUE)
  expect_match(output, "geom_histogram() +", fixed = TRUE)

  # correctness
  expect_match(output, "expression", fixed = TRUE)
  expect_match(output, "CO[2]~", fixed = TRUE)
})


<!-- README.md is generated from README.Rmd. Please edit that file -->

# evalthat

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/evalthat)](https://CRAN.R-project.org/package=evalthat)
<!-- badges: end -->

evalthat provides a testthat-style framework for LLM evaluation in R. If
you can write unit tests, you can compare performance across various
LLMs, improve your prompts using evidence, and quantify variability in
model output.

## Installation

You can install the development version of evalthat like so:

``` r
# install.packages("pak")
pak::pak("simonpcouch/evalthat")
```

## Example

evalthat code looks almost exactly like testthat code. Here’s an
example:

``` r
evaluating(
  model = "claude"
)

test_that("model can make a basic histogram", {
  ch <- elmer::chat_claude()
  input <- input(
    "Write ggplot code to plot a histogram of the mpg variable in mtcars. 
     Return only the plotting code, no backticks and no exposition."
  )
  
  output <- output(ch$chat(input))
  
  # check that output was syntactically code R code
  expect_r_code(output)
  
  # match keywords to affirm intended functionality
  expect_match(output, "ggplot(", fixed = TRUE)
  expect_match(output, "aes(", fixed = TRUE)
  expect_match(output, "geom_histogram(", fixed = TRUE)
  
  # flag output for manual grading
  target <- "ggplot(mtcars) + aes(x = mpg) + geom_histogram()"
  grade_human(input, output, target)
  
  # grade using an LLM---either instantaneously using the current model or
  # flag for later grading with a different model
  grade_model(input, output, target)
})
```

testthat users will notice a couple changes:

- The `evaluating()` function is sort of like `context()`, and logs
  metadata about the experiment.
- The functions `input()` and `output()` flag “what went into the
  model?” and “what came out?”
- In addition to the regular `expect_*()` functions from testthat, the
  package supplies a number of new expectation functions that are
  helpful in evaluating R code contained in a character string (as it
  will be when outputted from elmer or its extensions). Those that begin
  with `expect_*()` are automated, those that begin with `grade_*()` are
  less-so.

Running the above test file results in a persistent *result file*—think
of it like a snapshot. evalthat supplies a number of helpers for working
with result files, allowing you to compare performance across various
models, iterate on prompts, quantify variability in output, and so on.
On the full ggplot2 example file, we could run 5 passes evaluating
several different models for revising ggplot2 code:

``` r
library(elmer)

temp <- list(temperature = 1)

evaluate_across(
  "tests/evalthat/test-ggplot2.R",
  tibble(chat = c(
    chat_openai(model = "gpt-4o", api_args = temp, echo = FALSE),
    chat_openai(model = "gpt-4o-mini", api_args = temp, echo = FALSE),
    chat_claude(model = "claude-3-5-sonnet-latest", echo = FALSE))
  ),
  repeats = 5
)
```

![](inst/ex_eval.gif)

Then, `results_read()` will return a data frame with information on the
evaluation results for further analysis. Visualizing this example
output:

<img src="inst/ex_plot.png" alt="A ggplot2 histogram, showing distributions of performance on the task 'translating R erroring code to cli' for three different models: Claude 3.5 Sonner, GPT-4o, and GPT-4o-mini. Claude Sonnet 3.5 and GPT-4o with default elmer settings offer comparable performance on average, though GPT-4o's 'temperature' is such that the same input always returns the same response." width="100%" />

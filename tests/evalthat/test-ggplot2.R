function(chat = ellmer::chat_claude(echo = FALSE)) {
  skip_if_offline()

  chat$set_system_prompt(
    "When asked a question about R code, reply with only the code needed to answer
    the question. No exposition, no backticks."
  )

  test_that("model can write ggplot2 code for a basic histogram", {
    input <- input("Could you please write ggplot2 code to make a histogram of
                    the mpg variable from mtcars?")
    output <- output(chat$clone()$chat(input))

    # syntactically valid
    expect_r_code(output)

    # correct
    expect_match(output, "mtcars", fixed = TRUE)
    expect_match(output, "ggplot(", fixed = TRUE)
    expect_match(output, "aes(x = mpg", fixed = TRUE)
    expect_match(output, "geom_histogram(", fixed = TRUE)
  })

  test_that("model can convert code from base R `plot()`", {
    input <- input("Convert this code to use ggplot2: `boxplot(len ~ supp, data = ToothGrowth)`")
    output <- output(chat$clone()$chat(input))

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
    input <- input(
      "Make the bars side-by-side rather than stacked on top of each other:
       `ggplot(mtcars) + aes(x = cyl, fill = factor(vs)) + geom_bar()`
      "
    )
    output <- output(chat$clone()$chat(input))

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
    input <- input(
      "Make the bars skinnier:
       `ggplot(mtcars) + aes(x = cyl, fill = factor(vs)) + geom_bar()`
      "
    )
    output <- output(chat$clone()$chat(input))

    # syntactically valid
    expect_r_code(output)

    # leaves existing code as-is
    expect_match(output, "aes(x = cyl, fill = factor(vs))", fixed = TRUE)
    expect_match(output, "ggplot(mtcars)", fixed = TRUE)

    # correctness
    expect_match(output, "geom_bar(width = ", fixed = TRUE)
  })

  test_that("model can add means to a boxplot", {
    input <- input(
      "Add a square in each box to show the mean:
       `ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + geom_boxplot()`
      "
    )
    output <- output(chat$clone()$chat(input))

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
    input <- input(
      "Move the legend to the top left, slightly inside the grey pane:
       `ggplot(mtcars) + aes(x = cyl, fill = factor(vs)) + geom_bar()`
      "
    )
    output <- output(chat$clone()$chat(input))

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
    input <- input(
      "Make the 2 a subscript in the label:
       `ggplot(CO2) + aes(x = uptake) + geom_histogram() + labs(x = 'CO_2 Uptake')`
      "
    )
    output <- output(chat$clone()$chat(input))

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

  test_that("model can create dual y-axes with sec_axis", {
    input <- input(
     "Also show km per liter (multiply mpg by 0.425) on a second y axis:
      ggplot(mtcars) + aes(x = factor(cyl), y = mpg) + geom_boxplot()"
    )
    output <- output(chat$clone()$chat(input))

    # syntactically valid
    expect_r_code(output)

    # correctness
    expect_match(output, "scale_y_continuous", fixed = TRUE)
    expect_match(output, "sec.axis", fixed = TRUE)
    expect_match(output, "~.*0.425", perl = TRUE)
  })

  test_that("model can modify axis breaks to avoid overlap", {
    input <- input(
      "Rotate the x-axis labels by 45 degrees to avoid overlap:
     `ggplot(mtcars) + geom_bar(aes(factor(cyl))) + scale_x_discrete()`"
    )
    output <- output(chat$clone()$chat(input))

    expect_r_code(output)
    expect_match(output, "theme(", fixed = TRUE)
    expect_match(output, "axis.text.x = ", fixed = TRUE)
    expect_match(output, "angle = 45", fixed = TRUE)
  })

  test_that("model can add clockwise layout for polar coordinates", {
    input <- input(
      "Make this pie chart go clockwise instead of counter-clockwise:
     `ggplot(mtcars) + geom_bar(aes(x='', fill=factor(cyl))) + coord_polar(theta='y')`"
    )
    output <- output(chat$clone()$chat(input))

    expect_r_code(output)
    expect_match(output, "coord_polar", fixed = TRUE)
    expect_match(output, "direction", fixed = TRUE)
  })

  test_that("model can convert barplot from base R", {
    input <- input("Convert this code to ggplot2: barplot(table(mtcars$cyl))")
    output <- output(chat$clone()$chat(input))

    expect_r_code(output)

    expect_match(output, "ggplot", fixed = TRUE)
    expect_match(output, "mtcars", fixed = TRUE)
    expect_match(output, "factor(cyl)", fixed = TRUE)
    expect_match(output, "geom_bar(", fixed = TRUE)
  })

  test_that("model can convert overlaid histograms from base R", {
    input <- input("Convert this base R histogram to ggplot2:
                 hist(mtcars$mpg[mtcars$am==0], col=rgb(1,0,0,0.5))
                 hist(mtcars$mpg[mtcars$am==1], col=rgb(0,0,1,0.5), add=TRUE)")
    output <- output(chat$clone()$chat(input))

    expect_r_code(output)

    expect_match(output, "ggplot", fixed = TRUE)
    expect_match(output, "mtcars", fixed = TRUE)
    expect_match(output, "aes(", fixed = TRUE)
    expect_match(output, "fill = factor(am)", fixed = TRUE)
    expect_match(output, "geom_histogram", fixed = TRUE)
  })

  test_that("model can convert base R plot with facets", {
    input <- input("Convert this base R code to ggplot2:
                 par(mfrow=c(1,2))
                 plot(mpg ~ wt, data=subset(mtcars, am==0))
                 plot(mpg ~ wt, data=subset(mtcars, am==1))")
    output <- output(chat$clone()$chat(input))

    expect_r_code(output)

    expect_match(output, "ggplot", fixed = TRUE)
    expect_match(output, "mtcars", fixed = TRUE)
    expect_match(output, "aes(", fixed = TRUE)
    expect_match(output, "x = wt", fixed = TRUE)
    expect_match(output, "y = mpg", fixed = TRUE)
    expect_match(output, "facet_wrap|facet_grid", perl = TRUE)
  })

  test_that("model can diagnose %>% used in place of +", {
    input <- input("Fix this ggplot2 code:
                 ggplot(mtcars) %>% geom_point(aes(x = mpg, y = wt))")
    output <- output(chat$clone()$chat(input))

    expect_r_code(output)

    expect_match(output, " +", fixed = TRUE)
  })
}

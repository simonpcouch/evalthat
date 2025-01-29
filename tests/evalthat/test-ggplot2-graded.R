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

    grade_output(
      target = "ggplot(mtcars) + aes(x = mpg) + geom_histogram()"
    )
  })

  test_that("model can convert code from base R `plot()`", {
    input <- input("Convert this code to use ggplot2: `boxplot(len ~ supp, data = ToothGrowth)`")
    output <- output(chat$clone()$chat(input))

    grade_output(
      target = "ggplot(ToothGrowth) + aes(x = supp, y = len) + geom_boxplot()"
    )
  })

  test_that("model can convert from stacked to dodged bars", {
    input <- input(
      "Make the bars side-by-side rather than stacked on top of each other:
     `ggplot(mtcars) + aes(x = cyl, fill = factor(vs)) + geom_bar()`"
    )
    output <- output(chat$clone()$chat(input))

    grade_output(
      target = 'ggplot(mtcars) + aes(x = cyl, fill = factor(vs)) + geom_bar(position = "dodge")'
    )
  })

  test_that("model can decrease bar width", {
    input <- input(
      "Make the bars skinnier:
     `ggplot(mtcars) + aes(x = cyl, fill = factor(vs)) + geom_bar()`"
    )
    output <- output(chat$clone()$chat(input))

    grade_output(
      target = "ggplot(mtcars) + aes(x = cyl, fill = factor(vs)) + geom_bar(width = 0.5)"
    )
  })

  test_that("model can add means to a boxplot", {
    input <- input(
      "Add a square in each box to show the mean:
     `ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + geom_boxplot()`"
    )
    output <- output(chat$clone()$chat(input))

    grade_output(
      target = 'ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 15)'
    )
  })

  test_that("model can move a legend", {
    input <- input(
      "Move the legend to the top left, slightly inside the grey pane:
     `ggplot(mtcars) + aes(x = cyl, fill = factor(vs)) + geom_bar()`"
    )
    output <- output(chat$clone()$chat(input))

    grade_output(
      target = "ggplot(mtcars) + aes(x = cyl, fill = factor(vs)) + geom_bar() + theme(legend.position = c(0.1, 0.9))"
    )
  })

  test_that("model can use subscript in axis label", {
    input <- input(
      "Make the 2 a subscript in the label:
     `ggplot(CO2) + aes(x = uptake) + geom_histogram() + labs(x = 'CO_2 Uptake')`"
    )
    output <- output(chat$clone()$chat(input))

    grade_output(
      target = "
        ggplot(CO2) +
          aes(x = uptake) +
          geom_histogram() +
          labs(x = expression(CO[2]~'Uptake'))
      "
    )
  })

  test_that("model can create dual y-axes with sec_axis", {
    input <- input(
      "Also show km per liter (multiply mpg by 0.425) on a second y axis:
       ggplot(mtcars) + aes(x = factor(cyl), y = mpg) + geom_boxplot()"
    )
    output <- output(chat$clone()$chat(input))

    grade_output(
      target = '
        ggplot(mtcars) +
          aes(x = factor(cyl), y = mpg) +
          geom_boxplot() +
          scale_y_continuous(sec.axis = sec_axis(~.*0.425, name = "km/L"))
      '
    )
  })

  test_that("model can modify axis breaks to avoid overlap", {
    input <- input(
      "Rotate the x-axis labels by 45 degrees to avoid overlap:
       `ggplot(mtcars) + geom_bar(aes(factor(cyl))) + scale_x_discrete()`"
    )
    output <- output(chat$clone()$chat(input))

    grade_output(
      target = "
        ggplot(mtcars) +
          geom_bar(aes(factor(cyl))) +
          scale_x_discrete() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      "
    )
  })

  test_that("model can add clockwise layout for polar coordinates", {
    input <- input(
      "Make this pie chart go clockwise instead of counter-clockwise:
       `ggplot(mtcars) + geom_bar(aes(x='', fill=factor(cyl))) + coord_polar(theta='y')`"
    )
    output <- output(chat$clone()$chat(input))

    grade_output(
      target = '
        ggplot(mtcars) +
          geom_bar(aes(x="", fill=factor(cyl))) +
          coord_polar(theta="y", direction=-1)
      '
    )
  })

  test_that("model can convert barplot from base R", {
    input <- input("Convert this code to ggplot2: barplot(table(mtcars$cyl))")
    output <- output(chat$clone()$chat(input))

    grade_output(
      target = "ggplot(mtcars) + aes(x = factor(cyl)) + geom_bar()"
    )
  })

  test_that("model can convert overlaid histograms from base R", {
    input <- input("Convert this base R histogram to ggplot2:
               hist(mtcars$mpg[mtcars$am==0], col=rgb(1,0,0,0.5))
               hist(mtcars$mpg[mtcars$am==1], col=rgb(0,0,1,0.5), add=TRUE)")
    output <- output(chat$clone()$chat(input))

    grade_output(
      target = '
        ggplot(mtcars) +
          aes(x = mpg, fill = factor(am)) +
          geom_histogram(alpha = 0.5, position = "identity")
      '
    )
  })

  test_that("model can convert base R plot with facets", {
    input <- input("Convert this base R code to ggplot2:
               par(mfrow=c(1,2))
               plot(mpg ~ wt, data=subset(mtcars, am==0))
               plot(mpg ~ wt, data=subset(mtcars, am==1))")
    output <- output(chat$clone()$chat(input))

    grade_output(
      target = "
        ggplot(mtcars) +
          aes(x = wt, y = mpg) +
          geom_point() +
          facet_wrap(~am)
      "
    )
  })

  test_that("model can diagnose %>% used in place of +", {
    input <- input("Fix this ggplot2 code:
                    ggplot(mtcars) %>% geom_point(aes(x = mpg, y = wt))")
    output <- output(chat$clone()$chat(input))

    grade_output(
      target = "ggplot(mtcars) + geom_point(aes(x = mpg, y = wt))"
    )
  })
}

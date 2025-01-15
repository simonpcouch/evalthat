(function(something) {
  test_that("Success", {
    succeed()
  })

  test_that("Failure:1", {
    expect_true(FALSE)
  })

  test_that("Failure:2a", {
    f <- function() expect_true(FALSE)
    f()
  })
})(if (exists("something")) something else "important")

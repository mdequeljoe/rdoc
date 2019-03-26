context("options")

test_that("rdoc style options", {

  x <- rdoc_style()
  expect_true(is.list(x))

  x <- rdoc_style(title = crayon::blue)
  # check output

  x <- rdoc_style(title = NULL)
  expect_true(is.function(x$title))
  expect_equal(x$title("s"), "s")

  expect_error(rdoc_style(title = crayon::blue()))
  expect_error(rdoc_style(code_style = list(string = crayon::blue())))
  expect_error(rdoc_style(code_style = list(strings = crayon::blue)))

  x <- rdoc_text_formats()
  expect_true(is.list(x))

  x <- rdoc_text_formats(email = function(x) paste0("[email]", x))
  # check output

  expect_error(rdoc_text_formats(email = "email"))

})

context("options")

test_that("rdoc style options", {
  expect_true(is.list(rdoc_style()))
  expect_error(rdoc_style(title = crayon::blue()))
  expect_error(rdoc_style(code_style = list(string = crayon::blue())))
  expect_error(rdoc_style(code_style = list(strings = crayon::blue)))
  expect_true(is.list(rdoc_text_formats()))
  expect_error(rdoc_text_formats(email = "email"))
})

test_that("valid option inputs", {
  expect_true(is_valid_opt(list(
    x = function()
      1,
    y = NULL,
    z = list(function() {

    },
    function() {

    })
  )))
  expect_false(is_valid_opt(list(x = NULL, y = "A")))
})

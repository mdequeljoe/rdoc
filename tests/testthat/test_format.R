context("text formatting")

test_that("argument formatting", {
  at_ <- function(x)
    paste0("@", x, ">")

  expect_equal(format_args(" a sentence", at_),
               " a sentence")
  expect_equal(format_args("topic: character(1)", at_),
               "@topic>: character(1)")
  expect_equal(format_args(" topic_2: this: that:", at_),
               " @topic_2>: this: that:")
  expect_equal(format_args("   some words: abc", at_),
               "   some words: abc")
  expect_equal(format_args("...: dots", at_),
               "@...>: dots")
  expect_equal(format_args("na.rm: rm na", at_),
               "@na.rm>: rm na")

})

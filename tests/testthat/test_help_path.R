context("help path")

test_that("help path returns a string", {
  p <- help_path("min")
  expect_true(is.character(p))
  expect_equal(length(p), 1L)

  p <- help_path("not a function")
  expect_true(is.character(p))
  expect_equal(length(p), 0L)

})


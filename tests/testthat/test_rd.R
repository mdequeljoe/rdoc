context("rd")

cap_ <- capture.output

test_that("rd produces text output", {
  test_ <- function(x){
    expect_true(length(x) > 0L)
    expect_true(is.character(x))
  }
  x <- cap_(rd("min", by_section = FALSE))
  test_(x)

  xd <- cap_(rd_details("min", by_section = FALSE))
  test_(xd)
  expect_true(length(xd) < length(x))

  xe <- cap_(rd_example("min", by_section = FALSE))
  test_(xe)
  expect_true(length(xe) < length(x))

  x <- cap_(rd(min, by_section = FALSE))
  test_(x)

  fn <- "min"
  x <- cap_(rd(fn, by_section = FALSE))
  test_(x)

  pkg <- "base"
  x <- cap_(rd("min", by_section = FALSE, package = pkg))
  test_(x)

  x <- cap_(rd(min, by_section = FALSE, package = "base"))
  test_(x)
})

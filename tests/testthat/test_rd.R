context("rd")
options(rdoc.by_section = FALSE)
options(rdoc.header = TRUE)
cap_ <- capture.output
test_ <- function(x){
  expect_true(length(x) > 0L)
  expect_true(is.character(x))
  expect_true(grepl("min", x[1]))
  expect_true(grepl("\\{base\\}", x[1]))
}

test_that("rd produces text output", {

  x <- cap_(rd("min"))
  test_(x)

  xd <- cap_(rd_details("min"))
  test_(xd)
  expect_true(length(xd) < length(x))

  xe <- cap_(rd_example("min"))
  test_(xe)
  expect_true(length(xe) < length(x))

  x <- cap_(rd(min))
  test_(x)

  fn <- "min"
  x <- cap_(rd(fn))
  test_(x)

  pkg <- "base"
  x <- cap_(rd("min", package = pkg))
  test_(x)

  x <- cap_(rd_details(min, package = base))
  test_(x)

  x <- cap_(rd(min, package = "base"))
  test_(x)

})

test_that("rd_question produces text output", {

  x <- cap_(rd_question(min))
  test_(x)

  x <- cap_(rd_question(base::min))
  test_(x)

  x <- cap_(rd_question("min"))
  test_(x)
})

test_that("rdoc overrides ?", {
  rdoc()
  expect_true("rdoc" %in% searchpaths())
  x <- cap_(?min)
  test_(x)
  rdoc()
  expect_true(!"rdoc" %in% searchpaths())
})

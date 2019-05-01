context("rd")
cap_ <- capture.output
test_ <- function(x){
  expect_true(length(x) > 0L && is.character(x))
  expect_true(grepl("min", x[1]))
  expect_true(grepl("\\{base\\}", x[1]))
}
opts <- list(
  rdoc.by_section = FALSE,
  rdoc.header = TRUE
)

test_that("rd produces text output", {
  withr::with_options(
    opts,
    {
      expect_true(is.function(rd_()))
      x <- cap_(rdoc("min"))
      test_(x)

      xd <- cap_(rdoc_details("min"))
      test_(xd)
      expect_true(length(xd) < length(x))

      xe <- cap_(rdoc_examples("min"))
      test_(xe)
      expect_true(length(xe) < length(x))

      x <- cap_(rdoc(min))
      test_(x)

      fn <- "min"
      x <- cap_(rdoc(fn))
      test_(x)

      pkg <- "base"
      x <- cap_(rdoc("min", package = pkg))
      test_(x)

      x <- cap_(rdoc_details(min, package = base))
      test_(x)

      x <- cap_(rdoc(min, package = "base"))
      test_(x)

      x <- cap_(rdoc(rdoc_options))
      x_ <- cap_(rdoc("rdoc_options"))
      expect_equal(x, x_)
      expect_true(inherits(rdoc("function_is_not_found"), "help_files_with_topic"))
    }
  )
})

test_that("rd_question produces text output", {
  withr::with_options(opts, {
    x <- cap_(rdoc_question(min))
    test_(x)

    x <- cap_(rdoc_question(base::min))
    test_(x)

    x <- cap_(rdoc_question("min"))
    test_(x)

    expect_true(inherits(
      rdoc_question("function_is_not_found"),
      "help_files_with_topic"
    ))
  })
})

test_that("Rd files read", {
  withr::with_options(opts, {
    f <- system.file("extdata/rdoc_test.Rd", package = "rdoc")
    x <- cap_(rdoc_rd(f))
    expect_true(length(x) > 0L)
    expect_true(is.character(x))
  })
})

test_that("rdoc overrides ?", {
  use_rdoc()
  expect_true("rdoc" %in% searchpaths())
  expect_true(is.null(use_rdoc()))
  rm_rdoc()
  expect_true(!"rdoc" %in% searchpaths())
  expect_true(is.null(rm_rdoc()))
})

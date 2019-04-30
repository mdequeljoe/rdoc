context("formatting")

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

test_that("rdo formats", {

  f <- system.file("extdata/rdoc_test.Rd", package = "rdoc")
  x <- tools::parse_Rd(f)
  x <- rdoc:::format_rdo(x)
  expect_true(is.list(x$rdo))
  expect_equal(length(x$tables), 1)

})

test_that("setting rd titles", {

  ttl <- function(x) {
    paste0("_\b", paste0(strsplit(x, "")[[1]]),
           collapse = "")
  }

  o <- c(ttl("a long title"),
         ttl("split into multiple lines"),
         "",
         ttl("first section"),
         "",
         "some text")
  x <- set_rd_title(o)
  x <- gsub("_\b", "", x)
  expect_equal(x[1], "a long title split into multiple lines")
  expect_equal(x[2], "")
  expect_equal(x[3], "first section")
})

test_that("Rd table spacing passes", {
  txt <- "\\name{rdoc_test}
  \\title{table spacing}
  \\details{
    \\pkg{foo2} package \\tabular{rr}{
      1 \\tab 2\\cr3 \\tab 4} \\link[utils]{adist} reference 1
  }"
  write(txt, tf <- tempfile(fileext = '.Rd'))
  expect_output(rdoc_rd(tf))
})

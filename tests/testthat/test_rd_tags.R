context("rd_tags")

test_that("tags + subtags are found", {
  x <-
    structure(list(structure("a title", Rd_tag = "TEXT")), Rd_tag = "\\title")
  expect_equal(tag_(x), "\\title")
  expect_equal(subtags_(x), "TEXT")

  x <- structure(list(structure(list(), Rd_tag = "\\dots")), Rd_tag = "\\code")
  expect_equal(tag_(x), "\\code")
  expect_equal(subtags_(x), "\\dots")
  expect_equal(tag_("text"), character(1))

  x <- list(structure(list(
    structure(list(), Rd_tag = "\\R"),
    structure("docs", Rd_tag = "TEXT")
  ), Rd_tag = "\\title"))

  expect_equal(subtags_(x), c("\\R", "TEXT"))

})

test_that("format tags", {

  p <-
    structure(list(structure("pkgname", Rd_tag = "TEXT")), Rd_tag = "\\pkg")
  f <- function(x) sprintf("{%s}", x)
  o <- format_tag(p, f)
  expect_equal(tag_(o), "\\special")
  expect_equal(tag_(o[[1]]), "TEXT")
  expect_equal(unlist(o), "{pkgname}")

  o <- format_tag(p, NULL)
  expect_equal(o, p)
})

test_that("format inline code", {

  k <-
    structure(list(
      structure("options(rdoc.text_formats\n", Rd_tag = "RCODE"),
      structure("= rdoc_text_formats())", Rd_tag = "RCODE")
    ), Rd_tag = "\\code")

  o <- format_inline_code(k, styles = list(call = function(x) "CALL"))
  expect_equal(tag_(o), "TEXT")
  expect_equal(o[1], "CALL(rdoc.text_formats")

  k <- structure(list(structure("\"\\9\" & \"\\1\"", Rd_tag = "RCODE")), Rd_tag = "\\code")

  o <- format_inline_code(k, list(string = function(x) paste0("STRING", x)))
  expect_equal(tag_(o), "TEXT")
  expect_equal(o[1], "STRING\"\\\\9\" & STRING\"\\\\1\"")

  k <- structure(list(structure("foo()", Rd_tag = "RCODE"), structure("bar", Rd_tag = "TEXT")), Rd_tag = "\\code")
  o <- format_inline_code(k, list(call = function(x) "CALL"))
  expect_equal(o, k)

})

test_that("format links", {
  x <- structure(list(structure("adist", Rd_tag = "TEXT")),
            Rd_tag = "\\link",
            Rd_option = structure("utils", Rd_tag = "TEXT"))
  o <- format_link(x, function(x) x)
  expect_equal(o[1], "[adist](utils)")

  x <- structure(list(structure("range", Rd_tag = "TEXT")), Rd_tag = "\\link")
  o <- format_link(x, function(x) x)
  expect_equal(x, o)
})

test_that("format href", {
  x <- structure(list(list(structure("http://rstudio.com", Rd_tag = "VERB")),
                 list(structure("Rstudio", Rd_tag = "TEXT"))), Rd_tag = "\\href")
  o <- format_href(x, function(x) x)
  expect_equal(o[1], "[Rstudio](http://rstudio.com)")
})

test_that("format tables", {

  x <-
    structure(list(
      list(structure("rr", Rd_tag = "TEXT")),
      list(
        structure("\n", Rd_tag = "TEXT"),
        structure("1 ", Rd_tag = "TEXT"),
        structure(list(), Rd_tag = "\\tab"),
        structure(" 2", Rd_tag = "TEXT"),
        structure(list(), Rd_tag = "\\cr"),
        structure("\n", Rd_tag = "TEXT"),
        structure("3 ", Rd_tag = "TEXT"),
        structure(list(), Rd_tag = "\\tab"),
        structure(" 4\n", Rd_tag = "TEXT")
      )
    ), Rd_tag = "\\tabular")


  xt <- format_table(x, list(border_style = "none", width = 20))
  xt <- strsplit(xt, "\n")[[1]]
  expect_equal(xt,
               c("             ",
                 "             ",
                 "    1   2    ",
                 "             ",
                 "    3   4    ",
                 "             ",
                 "             "))

})


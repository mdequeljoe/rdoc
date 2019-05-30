context("rd")

test_ <- function(e){
  x <- capture.output(e)
  expect_true(length(x) > 0L && is.character(x))
  invisible(x)
}

opts <- list(
  rdoc.by_section = FALSE,
  rdoc.header = FALSE
)

test_that("rd produces text output", {
  withr::with_options(
    opts,
    {
      expect_true(is.function(rd_()))

      x <- test_(rdoc(min))
      xd <- test_(rdoc_details('min'))
      expect_true(length(xd) < length(x))

      xe <- test_(rdoc_examples('min'))
      expect_true(length(xe) < length(x))

      fn <- "min"
      test_(rdoc(fn))

      pkg <- "base"
      test_(rdoc("min", package = pkg))
      test_(rdoc(min, package = base))
      test_(rdoc(min, package = "base"))
      test_(rdoc(base::min))
    }
  )
})

test_that("rd_question produces text output", {
  withr::with_options(opts, {

    test_(rdoc_question(min))
    test_(rdoc_question(rdoc::rdoc))
    test_(rdoc_question("min"))
    test_(rdoc_question(base))
    test_(rdoc_question(package, utils))

  })
})

test_that("Rd files read", {
  withr::with_options(opts, {
    f <- system.file("extdata/rdoc_test.Rd", package = "rdoc")
    test_(rdoc_rd(f))
  })
})

test_that("rdoc overrides ?", {
  use_rdoc()
  expect_true("rdoc" %in% searchpaths())
  rm_rdoc()
  expect_true(!"rdoc" %in% searchpaths())
})

test_that('rdoc finds help', {
   h <- find_function('help', exclude = "devtools_shims|rdoc")
   expect_identical(h, utils::help)

   use_rdoc()
   h <- find_help(exclude = "devtools_shims|rdoc")
   expect_identical(h, utils::help)

   h <- find_help(exclude = NULL)
   expect_identical(h, rdoc)
})

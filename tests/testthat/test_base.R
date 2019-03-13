context("base R docs")
get_help_file <- getFromNamespace(".getHelpFile", "utils")
get_rdo <- function(topic, pkg = NULL){
  get_help_file(
    rdoc:::help_path(topic, package = pkg)
  )
}
check <- function(topic, pkg = NULL){
  capture.output(
    tools::Rd2txt(
      get_rdo(topic, pkg),
      options = list(width = getOption('width') - 3L)
    )
  )
}
rd_check <- function(topic){
  capture.output(
    rdoc::rd(topic, by_section = FALSE)
  )
}

test_pkg <- function(pkg, partial = FALSE, n = 100L) {

  pkg_exports <- ls(sprintf("package:%s", pkg))

  if (partial)
    pkg_exports <- pkg_exports[sample(seq_along(pkg_exports), n)]

  o <- lapply(pkg_exports, function(fn) {
    out <- tryCatch(
      capture.output(rd(
        fn, by_section = FALSE, package = pkg
      )),
      error = function(e)
        e,
      warning = function(w)
        w
    )
    passed <- !inherits(out, "condition")
    expect_true(passed)

    if (!passed) {
      print(fn)
      return(NULL)
    }

    orig_rdo <- check(fn, pkg)
    check_len <- length(out) >= length(orig_rdo)
    #expect_true(check_len)
    if (!check_len) {
      cat("\n", fn,
          "doesnt meet length check:\n original rd length = " ,
          length(orig_rdo),
          "\n converted rd length = ",
          length(out),
          "\n\n")
    }
  })

  invisible(NULL)
}

test_that("base package docs output without error or warning", {
  test_pkg("utils", TRUE, 50)
  test_pkg("base", TRUE, 50)
  test_pkg("stats", TRUE, 50)
})

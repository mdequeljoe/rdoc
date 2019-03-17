context("base R docs")
get_help_file <- getFromNamespace(".getHelpFile", "utils")

get_rdo <- function(topic, pkg = NULL) {
  get_help_file(rdoc:::help_path(topic, package = pkg))
}

check_original <- function(topic, pkg = NULL) {
  o <- get_rdo(topic, pkg)
  w <- getOption('width')
  capture.output(tools::Rd2txt(o, options = list(width = w)))
}

check_rd <- function(topic, pkg = NULL) {
  capture.output(rdoc::rd(topic, by_section = FALSE, package = pkg))
}

strip_lines <- function(x) {
  gsub("\\s|[[:punct:]]", "", x)
}

strip_rd <- function(x) {
  x <- strip_lines(x)
  paste(x, collapse = "")
}

compare_rd <- function(o, k) {
  o <- strip_rd(o)
  k <- crayon::strip_style(k)
  k <- strip_rd(k)
  k == o
}

test_pkg <- function(pkg, partial = FALSE, n = 100L) {
  pkg_exports <- ls(sprintf("package:%s", pkg))

  if (partial)
    pkg_exports <- pkg_exports[sample(seq_along(pkg_exports), n)]

  o <- lapply(pkg_exports, function(fn) {
    out <- tryCatch(
      check_rd(fn, pkg),
      error = function(e)
        e,
      warning = function(w)
        w
    )
    passed <- !inherits(out, "condition")
    expect_true(passed)

    if (!passed) {
      cat("\n", fn, "did not pass\n")
      return(NULL)
    }

    o <- check_original(fn, pkg)
    same_content <- compare_rd(o, out)
    expect_true(same_content)
    if (!same_content)
      cat("\n", fn, "differs in content\n")

  })

  invisible(NULL)
}

test_that("base package docs output without error or warning", {
  test_pkg("utils", TRUE, 30)
  test_pkg("base", TRUE, 30)
  test_pkg("stats", TRUE, 30)
})

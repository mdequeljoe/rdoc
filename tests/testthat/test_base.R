context("base R docs")
get_help_file <- getFromNamespace(".getHelpFile", "utils")
get_rdo <- function(topic, pkg){
  d <- as.call(list(utils::`help`, topic, pkg))
  d <- eval(d)[1]
  get_help_file(d)
}

check_original <- function(topic, pkg = NULL) {
  o <- get_rdo(topic, pkg)
  w <- getOption('width')
  capture.output(
    tools::Rd2txt(
      o,
      options = list(width = w, itemBullet = ">")
    )
  )
}

strip_lines <- function(x) {
  gsub("\\s|[[:punct:]]|_\\\b", "", x)
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

test_topic <- function(fn, pkg = NULL){
  out <- tryCatch(
    rdoc_text(fn, pkg),
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
  invisible(NULL)
}

test_pkg <- function(pkg, partial = FALSE, n = 100L, exclude = NULL) {
  pkg_exports <- ls(sprintf("package:%s", pkg))
  if (!is.null(exclude))
    pkg_exports <- pkg_exports[!pkg_exports %in% exclude]

  if (partial)
    pkg_exports <- pkg_exports[sample(seq_along(pkg_exports), n)]

  o <- lapply(pkg_exports, test_topic, pkg = pkg)
  invisible(NULL)
}

test_that("base package docs output without error or warning", {
  text_fmt <- rdoc_text_formats(
    table = NULL,
    href = NULL,
    link = NULL,
    email = NULL,
    url = NULL
  )
  withr::with_options(
    list(
      rdoc.header = FALSE,
      rdoc.by_section = FALSE,
      rdoc.text_formats = text_fmt,
      rdoc.item_bullet = ">"
    ),
    {
      test_pkg("utils", TRUE, 10)
      test_pkg("base", TRUE, 10, exclude = c("intToUtf8", "utf8ToInt"))
      test_pkg("stats", TRUE, 10)
    }
  )
})

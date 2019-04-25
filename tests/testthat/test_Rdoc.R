context("Rdoc")

test_that("selecting paths", {

  x <- c(
    "~/Library/dplyr/help/filter",
    "~/library/stats/help/filter"
  )
  mockery::stub(select_path, 'interactive', FALSE)
  p <- select_path(x, "filter")
  expect_equal(x[1], p)

  mockery::stub(select_path, 'interactive', TRUE)
  mockery::stub(select_path, 'readline', "2")
  sink(file = tempfile())
  p <- select_path(x, "filter")
  sink()
  expect_equal(x[2], p)
})


test_that("file show", {
  mockery::stub(show_file, "file.show", function(x) readLines(x))
  l <- Sys.getenv("LESS")
  s <- show_file(LETTERS)
  expect_equal(l, Sys.getenv("LESS"))
})

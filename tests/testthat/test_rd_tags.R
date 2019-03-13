context("rd_tags")

test_that("subtags are found", {
  k <- list(list())
  attr(k[[1]], "Rd_tag") <- "\\dots"
  attr(k, "Rd_tag") <- "\\code"
  s <- get_subtags(k)
  expect_equal("\\dots", s)
})


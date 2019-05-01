context("line reflow")

test_that("spaces find correct positions", {

  expect_equal(spaces("hello"), numeric(0))
  expect_true(spaces("hello world") ==  6)
  expect_true(all(spaces("h w !") == c(2, 4)))

})

test_that("works with indentation", {

  expect_false(nzchar(ind("hi")))
  expect_equal(" ", ind(" hi"))
  expect_equal("hi", rm_ind(" hi"))
  x <- "     A sentence, "
  expect_equal("     ", ind(x))
  expect_equal("A sentence, ", rm_ind(x))
})

test_that("line reflow works as expected", {
  x <- c("1 2 3 4",
         "1",
         "2",
         "3",
         "4")
  xr <- reflow_lines(x)
  expect_equal(xr, c("1 2 3 4", "1 2 3 4"))

  x <- c("",
         "1 2",
         "1",
         "2",
         "")
  xr <- reflow_lines(x)
  expect_equal(xr, c("", "1 2", "1 2", ""))

  x <- c("1 2",
         "1",
         "",
         "2")
  xr <- reflow_lines(x)
  expect_equal(xr, x)

  x <- c("1 2 3 9",
         "1 2 3",
         "4 5")
  xr <- reflow_lines(x)
  expect_equal(xr, c("1 2 3 9", "1 2 3 4", "5"))

  xr <- reflow_lines(x, exclude = "4")
  expect_equal(xr, c("1 2 3 9", "1 2 3", "4 5"))

  x <- c("a b c d",
         crayon::red("a b"),
         "c d e")
  x2 <- reflow_lines(x)
  expect_equal(x2[2], paste(x[2], "c d"))

})


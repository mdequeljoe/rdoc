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

  x <- c("1 2 3 4",
         "1 2 3",
         "4 5")
  xr <- reflow_lines(x)
  expect_equal(xr, c("1 2 3 4", "1 2 3 4", "5"))

})

test_that("line reflow with styles works as expected", {

  x <- c("     The minimum and maximum of a numeric empty set are \033[32m+\033[39m\033[34mInf\033[39m and",
         "     \033[32m-\033[39m\033[34mInf\033[39m (in this order!) which ensures \033[3mtransitivity\033[23m, e.g.,",
         "     \033[36mmin\033[39m(x1, \033[36mmin\033[39m(x2)) \033[32m==\033[39m \033[36mmin\033[39m(x1, x2).  For numeric x",
         "     \033[36mmax\033[39m(x) \033[32m==\033[39m \033[32m-\033[39m\033[34mInf\033[39m and \033[36mmin\033[39m(x)",
         "     \033[32m==\033[39m \033[32m+\033[39m\033[34mInf\033[39m whenever \033[36mlength\033[39m(x) \033[32m==\033[39m",
         "     \033[34m0\033[39m (after removing missing values if requested).  However, pmax and pmin",
         "     return \033[34mNA\033[39m if all the parallel elements are \033[34mNA\033[39m even for na.rm",
         "     \033[32m=\033[39m \033[34mTRUE\033[39m.")
  x2 <- reflow_lines(x)
  expect_true(length(x2) < length(x))
  m <- max(nchar(x))
  m2 <- max(nchar(crayon::strip_style(x2)))
  expect_equal(m, m2)
  x <- gsub("\\s", "", paste(x, collapse = ""))
  x2 <- gsub("\\s", "", paste(x, collapse = ""))
  expect_equal(x, x2)
})






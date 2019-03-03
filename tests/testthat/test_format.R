context("text formatting")

# formatting:
# need a convention to resolve ambiguity of:
# foo_bar is a fn. foo_bar does something.
# dont know if it is foo<italics: bar is...> or foo_bar is ...

# if format starts within a word: it must end at the end of the word
# if format starts outside of word - must end outside of any word

test_that("formatting spotted", {

  v <- c(
    "a_b_",
    "a_b",
    "_ab",
    "_a_b_"
  )
  expect_true(has_italic(v))
  expect_false(has_italic(v[2L]))
  expect_true(has_italic(v[3L]))
  expect_true(has_italic(v[4L]))

})


test_that("formatting conventions hold", {

  v <- c(
    "",
    "foo_bar returns a dataframe. foo_bar does ...",
    "foo_bar_ is left as is"
  )
  expect_true(has_italic(v))


  vf <- fmt_italic(v)
  expect_equal(length(vf), length(v))
  expect_identical(vf, v)

  v <- c(
    "",
    "_foo_bar returns a dataframe. foo_bar does ..._",
    "notice the suffix foo__bar_",
    "foo_bar_baz is another fn.",
    "_foo_bar_baz_ is another fn.",
    "foo__bar__baz is another fn"
  )
  vf <- fmt_italic(v)
  expect_equal(length(vf), length(v))
  expect_equal(
    vf[2L],
    "\033[3mfoo_bar returns a dataframe. foo_bar does ...\033[23m"
  )
  expect_equal(
    vf[3L],
    "notice the suffix foo_\033[3mbar\033[23m"
  )
  expect_equal(
    vf[4L],
    "foo_bar_baz is another fn."
  )
  expect_equal(
    vf[5L],
    "\033[3mfoo_bar_baz\033[23m is another fn."
  )
  expect_equal(
    vf[6L],
    "foo_\033[3mbar\033[23m_baz is another fn"
  )
  #italic symbols separated on different lines
  v <- c(
    "",
    "foo_bar does something._",
    "And returns an integer_"
  )
  vf <- fmt_italic(v)
  expect_equal(length(vf), length(v))
  expect_equal(
    vf[2L],
    "foo_bar does something.\033[3m"
  )
  expect_equal(
    vf[3L],
    "And returns an integer\033[23m"
  )

})


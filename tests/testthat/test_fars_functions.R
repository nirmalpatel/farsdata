test_that("fars_read() works", {
  x <- system.file("extdata", "accident_2013.csv.bz2", package = "farsdata")
  y <- system.file("extdata", "accident_WRONG.csv.bz2", package = "farsdata")

  expect_that(fars_read(x), is_a("tbl_df"))
  expect_that(fars_read(y), throws_error())
})

test_that("make_filename() works", {
  x <- make_filename(2005)
  y <- make_filename(2006)

  expect_that(x, is_identical_to("accident_2005.csv.bz2"))
  expect_that(y, is_identical_to("accident_2006.csv.bz2"))
})

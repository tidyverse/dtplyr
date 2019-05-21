context("sample")

test_that("sample preserves class", {
  skip_if_dtplyr()

  # the basics don't work anymore here: sample_n(data.table(mtcars), 1)

  expect_is(sample_n(data.table(mtcars), 1), "data.table")
  expect_is(sample_n(tbl_dt(mtcars), 1), "tbl_dt")
  expect_is(sample_n(group_by(data.table(mtcars), mpg), 1), "tbl_dt")

  expect_is(sample_frac(data.table(mtcars), 1), "data.table")
  expect_is(sample_frac(tbl_dt(mtcars), 1), "tbl_dt")
  expect_is(sample_frac(group_by(data.table(mtcars), mpg), 1), "tbl_dt")
})


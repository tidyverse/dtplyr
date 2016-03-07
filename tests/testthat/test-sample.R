context("sample")

test_that("sample preserves class", {
  expect_is(sample_n(data.table(mtcars), 1), "data.table")
  expect_is(sample_n(tbl_dt(mtcars), 1), "tbl_dt")

  expect_is(sample_frac(data.table(mtcars), 1), "data.table")
  expect_is(sample_frac(tbl_dt(mtcars), 1), "tbl_dt")
})


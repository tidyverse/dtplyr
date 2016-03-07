context("Distinct")

dt <- data.table(
  x = c(1, 1, 1, 1),
  y = c(1, 1, 2, 2),
  z = c(1, 1, 2, 2)
)

test_that("distinct removes duplicates (data.table)", {
  res <- distinct(dt, x)
  expect_s3_class(res, "data.table")
  expect_equal(nrow(res), 1)
})

test_that("distinct removes duplicates (tbl_dt)", {
  res <- distinct(tbl_dt(dt), x)
  expect_s3_class(res, c("tbl_dt", "data.table"))
  expect_equal(nrow(res), 1)
})


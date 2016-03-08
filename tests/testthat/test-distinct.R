context("Distinct")

dt <- data.table(
  x = c(1, 1, 1, 1),
  y = c(1, 1, 2, 2),
  z = c(1, 1, 2, 2)
)

test_that("distinct removes duplicates (data.table)", {
  res <- distinct(dt, x)
  expect_is(res, "data.table")
  expect_equal(nrow(res), 1)
})

test_that("distinct removes duplicates (tbl_dt)", {
  res <- distinct(tbl_dt(dt), x)
  expect_is(res, c("tbl_dt", "data.table"))
  expect_equal(nrow(res), 1)
})

test_that("grouped_by uses grouping vars & preserves groups", {
  res <- dt %>% group_by(x) %>% distinct(y)

  expect_is(res, "grouped_dt")
  expect_equal(res$x, c(1, 1))
  expect_equal(res$y, c(1, 2))
})

test_that("distinct works when key is set", {
  dt <- copy(dt)
  setkey(dt, x)

  res <- distinct(dt, x)
  expect_equal(nrow(res), 1)
})

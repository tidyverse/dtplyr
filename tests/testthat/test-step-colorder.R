test_that("can reorder columns", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1), "DT")

  expect_equal(
    dt %>% step_colorder(c("y", "x")) %>% show_query(),
    expr(setcolorder(copy(DT), !!c("y", "x")))
  )

  expect_named(
    dt %>% step_colorder(c("y", "x")) %>% collect(),
    c("y", "x")
  )

  expect_equal(
    dt %>% step_colorder(c(2L, 1L)) %>% show_query(),
    expr(setcolorder(copy(DT), !!c(2L, 1L)))
  )

  expect_named(
    dt %>% step_colorder(c(2L, 1L)) %>% collect(),
    c("y", "x")
  )
})

test_that("can handle duplicate column names", {
  dt <- lazy_dt(data.table(x = 3, x = 2, y = 1), "DT")

  expect_snapshot_error(dt %>% step_colorder(c("y", "x")))

  expect_equal(
    dt %>% step_colorder(c(3L, 2L)) %>% show_query(),
    expr(setcolorder(copy(DT), !!c(3L, 2L)))
  )

  expect_equal(
    dt %>% step_colorder(c(3L, 2L)) %>% as.data.table(),
    data.table(y = 1, x = 2, x = 3)
  )
})

test_that("checks col_order", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1), "DT")

  expect_snapshot_error(dt %>% step_colorder(c("y", "y")))
  expect_snapshot_error(dt %>% step_colorder(c(1L, 1L)))
})

test_that("works for empty input", {
  dt <- lazy_dt(data.frame(x = 1), "DT")

  expect_equal(dt %>% step_colorder(character()), dt)
  expect_equal(dt %>% step_colorder(integer()), dt)
})

test_that("doesn't add step if not necessary", {
  dt <- lazy_dt(data.frame(x = 1, y = 2), "DT")

  expect_equal(dt %>% step_colorder(c("x", "y")), dt)
  expect_equal(dt %>% step_colorder("x"), dt)

  expect_equal(dt %>% step_colorder(1:2), dt)
  expect_equal(dt %>% step_colorder(1L), dt)
})

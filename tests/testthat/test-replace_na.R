# lazy data.tables -----------------------------------------------------------

test_that("empty call does nothing", {
  tbl <- tibble(x = c(1, NA))
  dt <- lazy_dt(tbl, "DT")
  out <- collect(replace_na(dt))
  expect_equal(out, tbl)
})

test_that("missing values are replaced", {
  tbl <- tibble(x = c(1, NA))
  dt <- lazy_dt(tbl, "DT")
  step <- replace_na(dt, list(x = 0))
  out <- collect(step)
  expect_equal(show_query(step), expr(copy(DT)[, `:=`(x = fcoalesce(x, 0))]))
  expect_equal(out$x, c(1, 0))
})

test_that("don't complain about variables that don't exist", {
  tbl <- tibble(a = c(1, NA))
  dt <- lazy_dt(tbl, "DT")
  out <- collect(replace_na(dt, list(a = 100, b = 0)))
  expect_equal(out, tibble(a = c(1, 100)))
})

# Inside mutate() -----------------------------------------------------------

test_that("missing values are replaced", {
  tbl <- tibble(x = c(1, NA))
  dt <- lazy_dt(tbl, "DT")
  step <- mutate(dt, x = replace_na(x, 0))
  out <- collect(step)
  expect_equal(show_query(step), expr(copy(DT)[, `:=`(x = fcoalesce(x, 0))]))
  expect_equal(out$x, c(1, 0))
})

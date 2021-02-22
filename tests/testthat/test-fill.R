test_that("all missings left unchanged", {
  tbl <- tibble(
    int = c(NA_integer_, NA),
    dbl = c(NA_real_, NA),
  )
  dt <- lazy_dt(tbl, "DT")

  down <- fill(dt, int, dbl)
  up <- fill(dt, int, dbl, .direction = "up")

  expect_identical(collect(down), tbl)
  expect_identical(collect(up), tbl)
})

test_that("missings are filled correctly & translations are correct", {
  tbl <- tibble(x = c(NA, 1, NA, 2, NA, NA))
  dt <- lazy_dt(tbl, "DT")

  step <- fill(dt, x)
  expect_equal(show_query(step), expr(copy(DT)[, `:=`(x = nafill(x, "locf"))]))
  expect_equal(collect(step)$x, c(NA, 1, 1, 2, 2, 2))

  step <- fill(dt, x, .direction = "up")
  expect_equal(show_query(step), expr(copy(DT)[, `:=`(x = nafill(x, "nocb"))]))
  expect_equal(collect(step)$x, c(1, 1, 2, 2, NA, NA))

  step <- fill(dt, x, .direction = 'downup')
  expect_equal(show_query(step), expr(copy(DT)[, `:=`(x = nafill(nafill(x, "locf"), "nocb"))]))
  expect_equal(collect(step)$x, c(1, 1, 1, 2, 2, 2))

  step <- fill(dt, x, .direction = 'updown')
  expect_equal(show_query(step), expr(copy(DT)[, `:=`(x = nafill(nafill(x, "nocb"), "locf"))]))
  expect_equal(collect(step)$x, c(1, 1, 2, 2, 2, 2))
})

test_that("auto-conversion to lazy_dt works as intended", {
  dt <- data.table(x = c(NA, 1, NA, 2, NA, NA))

  out <- collect(fill(dt, x))
  expect_equal(out$x, c(NA, 1, 1, 2, 2, 2))
})

test_that("missings filled down for each atomic vector", {
  tbl <- tibble(
    int = c(1L, NA),
    dbl = c(1, NA)
  )
  dt <- lazy_dt(tbl, "DT")

  out <- collect(fill(dt, tidyselect::everything()))
  expect_equal(out$int, c(1L, 1L))
  expect_equal(out$dbl, c(1, 1))
})

test_that("missings filled up for each vector", {
  tbl <- tibble(
    int = c(NA, 1L),
    dbl = c(NA, 1)
  )
  dt <- lazy_dt(tbl, "DT")

  out <- collect(fill(dt, tidyselect::everything(), .direction = "up"))
  expect_equal(out$int, c(1L, 1L))
  expect_equal(out$dbl, c(1, 1))
})

test_that("fill respects grouping", {
  tbl <- tibble(x = c(1, 1, 2), y = c(1, NA, NA))
  dt <- lazy_dt(tbl, "DT")
  out <- dt %>% dplyr::group_by(x) %>% fill(y) %>% collect()
  expect_equal(out$y, c(1, 1, NA))
})

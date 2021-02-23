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

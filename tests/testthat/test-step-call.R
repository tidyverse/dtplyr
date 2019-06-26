
# head and tail -------------------------------------------------------------

test_that("simple calls generate expected results", {
  dt <- lazy_dt(data.table(x = 1), "DT")

  expect_equal(
    dt %>% head() %>% show_query(),
    expr(head(DT, n = 6L))
  )
  expect_equal(
    dt %>% tail() %>% show_query(),
    expr(tail(DT, n = 6L))
  )
})


# distinct ----------------------------------------------------------------

test_that("no input uses all variables", {
  dt <- lazy_dt(data.table(x = c(1, 1), y = c(1, 2)), "dt")

  expect_equal(
    dt %>% distinct() %>% show_query(),
    expr(unique(dt))
  )
})

test_that("uses supplied variables", {
  dt <- lazy_dt(data.table(x = c(1, 1), y = c(1, 2)), "dt")

  expect_equal(
    dt %>% distinct(y) %>% show_query(),
    expr(unique(dt[, .(y)]))
  )

  expect_equal(
    dt %>% group_by(x) %>% distinct(y) %>% show_query(),
    expr(unique(dt[, .(x, y)]))
  )
})

test_that("doesn't duplicate variables", {
  dt <- lazy_dt(data.table(x = c(1, 1), y = c(1, 2)), "dt")

  expect_equal(
    dt %>% distinct(x, x) %>% show_query(),
    expr(unique(dt[, .(x)]))
  )

  expect_equal(
    dt %>% group_by(x) %>% distinct(x) %>% show_query(),
    expr(unique(dt[, .(x)]))
  )
})
test_that("keeps all variables if requested", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "dt")

  expect_equal(
    dt %>% distinct(y, .keep_all = TRUE) %>% show_query(),
    expr(unique(dt, by = "y"))
  )

  expect_equal(
    dt %>% group_by(x) %>% distinct(y, .keep_all = TRUE) %>% show_query(),
    expr(unique(dt, by = c("x", "y")))
  )
})

test_that("can compute distinct computed variables", {
  dt <- lazy_dt(data.table(x = c(1, 1), y = c(1, 2)), "dt")

  expect_equal(
    dt %>% distinct(z = x + y) %>% show_query(),
    expr(unique(dt[, .(z = x + y)]))
  )

  expect_equal(
    dt %>% distinct(z = x + y, .keep_all = TRUE) %>% show_query(),
    expr(unique(copy(dt)[, `:=`(z = x + y)], by = "z"))
  )
})

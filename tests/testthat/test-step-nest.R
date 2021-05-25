test_that("nest turns grouped values into one list-df", {
  ldt <- lazy_dt(tibble(x = c(1, 1, 1), y = 1:3), "DT")
  out <- nest(ldt, data = y)
  outc <- collect(out)

  expect_equal(show_query(out), expr(DT[, .(data = .(.SD)), by = .(x)]))

  expect_equal(group_vars(out), character())
  expect_equal(out$vars, c("x", "data"))

  expect_equal(outc$x, 1)
  expect_equal(length(outc$data), 1L)
  expect_equal(outc$data[[1L]], data.table(y = 1:3))
})

test_that("nest uses grouping vars if present", {
  ldt <- lazy_dt(tibble(x = c(1, 1, 1), y = 1:3), "DT")
  out <- nest(dplyr::group_by(ldt, x))

  expect_equal(group_vars(out), "x")
  expect_equal(show_query(out), expr(DT[, .(data = .(.SD)), by = .(x)]))
})

test_that("provided grouping vars override grouped defaults", {
  ldt <- tibble(x = 1, y = 2, z = 3) %>% group_by(x) %>% lazy_dt("DT")
  out <- nest(ldt, data = y)

  expect_equal(show_query(out), expr(DT[, .(data = .(.SD)), by = .(x, z)]))
  expect_equal(group_vars(out), "x")
  expect_equal(out$vars, c("x", "z", "data"))
})

test_that("puts data into the correct row", {
  ldt <- tibble(x = 1:3, y = c("B", "A", "A")) %>% lazy_dt()
  out <- nest(ldt, data = x) %>% collect() %>% dplyr::filter(y == "B")
  expect_equal(out$data[[1]]$x, 1)
})

test_that("nesting everything yields a simple data frame", {
  dt <- data.table(x = 1:3, y = c("B", "A", "A"))
  ldt <- lazy_dt(dt, "DT")
  out <- nest(ldt, data = c(x, y))

  expect_equal(show_query(out), expr(DT[, .(data = .(.SD))]))
  expect_equal(out$vars, "data")

  expect_equal(collect(out)$data, list(dt))
})

test_that("nest preserves order of data", {
  ldt <- lazy_dt(tibble(x = c(1, 3, 2, 3, 2), y = 1:5), "DT")
  out <- nest(ldt, data = y)
  expect_equal(collect(out)$x, c(1, 3, 2))
})

test_that("can strip names", {
  ldt <- lazy_dt(tibble(x = c(1, 1, 1), ya = 1:3, yb = 4:6), "DT")
  out <- nest(ldt, y = starts_with("y"), .names_sep = "")

  expect_equal(
    show_query(out),
    expr(DT[, .(y = .(data.table(a = ya, b = yb))), by = .(x)])
  )

  expect_named(collect(out)$y[[1]], c("a", "b"))
})

test_that("can nest multiple columns", {
  ldt <- lazy_dt(tibble(x = 1, a1 = 1, a2 = 2, b1 = 1, b2 = 2), "DT")
  out <- ldt %>% nest(a = c(a1, a2), b = c(b1, b2))

  expect_equal(
    show_query(out),
    expr(DT[, .(a = .(data.table(a1, a2)), b = .(data.table(b1, b2))), by = .(x)])
  )
  expect_equal(out$vars, c("x", "a", "b"))
})

test_that("nesting no columns nests all inputs", {
  # included only for backward compatibility
  ldt <- lazy_dt(tibble(a1 = 1, a2 = 2, b1 = 1, b2 = 2), "DT")
  expect_warning(out <- nest(ldt), "must not be empty")
  expect_equal(show_query(out), expr(DT[, .(data = .(.SD))]))
})
